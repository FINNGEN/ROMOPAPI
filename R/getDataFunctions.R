#' Get code counts and related concept information
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Vector of concept IDs to get counts and relationships for
#'
#' @return A list containing:
#' \itemize{
#'   \item concept_relationships - Tibble of concept relationships (Maps to, Subsumes)
#'   \item concepts - Tibble of concept details for related concepts
#'   \item code_counts - Tibble of code counts from the code_counts table
#' }
#'
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom tibble as_tibble
#' @importFrom dplyr pull
#'
#' @export
getCodeCounts <- function(
    CDMdbHandler,
    conceptIds) {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")

    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

  
    #
    # FUNCTION
    #
    
    # - Get concept parents and all descendants, only if they have code counts
    sql <- "
    -- Get parents of the conceptIds
    SELECT DISTINCT
        ca.descendant_concept_id AS concept_id_1,
        ca.ancestor_concept_id AS concept_id_2,
        'Parent' AS relationship_id
    FROM @vocabularyDatabaseSchema.concept_ancestor ca
    WHERE descendant_concept_id IN (@conceptIds)
    AND min_levels_of_separation = 1

    UNION ALL

    SELECT DISTINCT
        ca.ancestor_concept_id as concept_id_1,
        ca.descendant_concept_id as concept_id_2,
        CASE 
            WHEN min_levels_of_separation = 0 THEN 'Root'
            ELSE CAST(min_levels_of_separation AS VARCHAR) || '-' || CAST(max_levels_of_separation AS VARCHAR)
        END AS relationship_id
    FROM @vocabularyDatabaseSchema.concept_ancestor ca
    -- Get only the descendants that have code counts
    INNER JOIN @resultsDatabaseSchema.code_counts cc
    ON ca.descendant_concept_id = cc.concept_id
    --
    WHERE ancestor_concept_id IN (@conceptIds)
    ORDER BY relationship_id;
    "

    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, conceptIds = paste(conceptIds, collapse = ","), resultsDatabaseSchema = resultsDatabaseSchema)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    parentAndDescendants <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble()

    parentAndDescendantsConceptIds <- parentAndDescendants |>
        dplyr::pull(concept_id_2) |>
        unique()

    # - Get mapped concepts to the parent and descendants, only if they have code counts
    # Get all 'Maps to', 'Mapped from', for the family concepts
    sql <- "SELECT DISTINCT
        concept_id_1,
        concept_id_2,
        relationship_id
    FROM @vocabularyDatabaseSchema.concept_relationship cr
    -- Get only the descendants that have code counts
    INNER JOIN @resultsDatabaseSchema.code_counts cc
    ON cr.concept_id_2 = cc.concept_id
    --
    WHERE relationship_id IN ('Maps to','Mapped from') 
    AND concept_id_1 != concept_id_2
    AND concept_id_1 IN (@parentAndDescendantsConceptIds);"
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, resultsDatabaseSchema = resultsDatabaseSchema,
     parentAndDescendantsConceptIds = paste(parentAndDescendantsConceptIds, collapse = ","))
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    concept_relationships <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble()

    concept_relationships = dplyr::bind_rows(parentAndDescendants, concept_relationships)

    parentAndDescendantsAndMappedConceptIds <- concept_relationships |>
        dplyr::pull(concept_id_2) |>
        unique()
 
    # - Get code counts
    # Get all the code counts for the descendantConceptIds
    sql <- "SELECT * FROM @resultsDatabaseSchema.code_counts WHERE concept_id IN (@parentAndDescendantsAndMappedConceptIds);"
    sql <- SqlRender::render(sql, resultsDatabaseSchema = resultsDatabaseSchema, parentAndDescendantsAndMappedConceptIds = paste(parentAndDescendantsAndMappedConceptIds, collapse = ","))
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    code_counts <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble() |>
        dplyr::select(-domain)

    # - Get concept details
    sql <- "SELECT * FROM @vocabularyDatabaseSchema.concept WHERE concept_id IN (@parentAndDescendantsAndMappedConceptIds);"
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, parentAndDescendantsAndMappedConceptIds = paste(parentAndDescendantsAndMappedConceptIds, collapse = ","))
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    concepts <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble()


    return(list(
        concept_relationships = concept_relationships,
        code_counts = code_counts,
        concepts = concepts
    ))
}


#' Get the CDM source information
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#'
#' @return A tibble of the CDM source information
#'
#' @export
getCDMSource <- function(
    CDMdbHandler) {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")

    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    sql <- "SELECT * FROM @vocabularyDatabaseSchema.cdm_source;"
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    cdm_source <- DatabaseConnector::dbGetQuery(connection, sql) |> tibble::as_tibble()

    return(cdm_source)
}


#' Get list of concepts with code counts
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#'
#' @return A tibble with concept_id, concept_name, vocabulary_id, and standard_concept for concepts present in code_counts
#'
#' @importFrom checkmate assertClass
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom tibble as_tibble
#' @export
getListOfConcepts <- function(
    CDMdbHandler) {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")

    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #
    # Get concept_id, concept_name, vocabulary_id, standard_concept for all concept_ids present in code_counts in one SQL call
    sql <- "
    SELECT DISTINCT c.concept_id, c.concept_name, c.vocabulary_id, c.standard_concept
       FROM @vocabularyDatabaseSchema.concept c
       INNER JOIN @resultsDatabaseSchema.code_counts cc
       ON c.concept_id = cc.concept_id;"
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, resultsDatabaseSchema = resultsDatabaseSchema)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    concepts <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble() |>
        dplyr::mutate(standard_concept = dplyr::if_else(is.na(standard_concept), TRUE, FALSE))

    return(concepts)
}