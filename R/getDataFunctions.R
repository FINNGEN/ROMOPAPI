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
    # TEMP: only one conceptId at the moment
    conceptIds |> checkmate::assertIntegerish(min.len = 1, max.len = 1)


    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    browser()
    #
    # FUNCTION
    #

    # - Get concept parents and all descendants
    sql <- "
    -- Get all descendants of the conceptIds recursively
    WITH RECURSIVE concept_tree AS (
        SELECT 
            ancestor_concept_id,
            descendant_concept_id,
            1 as level
        FROM @vocabularyDatabaseSchema.concept_ancestor
        WHERE ancestor_concept_id IN (@conceptIds)
        AND min_levels_of_separation = 1

        UNION ALL

        SELECT 
            ca.ancestor_concept_id,
            ca.descendant_concept_id,
            ct.level + 1
        FROM @vocabularyDatabaseSchema.concept_ancestor ca
        INNER JOIN concept_tree ct ON ca.ancestor_concept_id = ct.descendant_concept_id
        WHERE ca.min_levels_of_separation = 1
    )
    SELECT DISTINCT
        ancestor_concept_id,
        descendant_concept_id,
        level
    FROM concept_tree ct
    
    UNION ALL
    
    -- Get parents of the conceptIds
    SELECT DISTINCT
        ancestor_concept_id,
        descendant_concept_id,
        -1 as level
    FROM @vocabularyDatabaseSchema.concept_ancestor
    WHERE descendant_concept_id IN (@conceptIds)
    AND min_levels_of_separation = 1

    UNION ALL

    -- Gets itself
    SELECT DISTINCT
        concept_id,
        concept_id,
        0 as level
    FROM @vocabularyDatabaseSchema.concept
    WHERE concept_id IN (@conceptIds)

    ORDER BY level;
    "

    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, conceptIds = paste(conceptIds, collapse = ","), resultsDatabaseSchema = resultsDatabaseSchema)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    recursiveDescendants <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble()

    familyConceptIds <- recursiveDescendants |>
        dplyr::pull(descendant_concept_id) |>
        unique()

    # - Get concept_relationships
    # Get all 'Maps to', 'Mapped from', for the family concepts
    sql <- "SELECT 
        concept_id_1,
        concept_id_2,
        relationship_id
    FROM @vocabularyDatabaseSchema.concept_relationship
    WHERE relationship_id IN ('Maps to','Mapped from') 
    AND concept_id_1 != concept_id_2
    AND concept_id_1 IN (@familyConceptIds);"
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, familyConceptIds = paste(familyConceptIds, collapse = ","))
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    concept_relationships <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble()


    descendantConceptIds <- c(
        concept_relationships |>
            dplyr::pull(concept_id_2),
        concept_relationships |>
            dplyr::pull(concept_id_1)
    ) |>
        unique()

    # - Get code counts
    # Get all the code counts for the descendantConceptIds
    sql <- "SELECT * FROM @resultsDatabaseSchema.code_counts WHERE concept_id IN (@descendantConceptIds);"
    sql <- SqlRender::render(sql, resultsDatabaseSchema = resultsDatabaseSchema, descendantConceptIds = paste(familyConceptIds, collapse = ","))
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    code_counts <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble() |>
        dplyr::select(-domain)

    # if the code_count is 0 for 'Mapped from' or 'Mapped to', remove from concept_relationships
    concept_relationships <- concept_relationships |>
        dplyr::semi_join(
            code_counts,
            by = c("concept_id_2" = "concept_id")
        )

    descendantConceptIds <- c(
        concept_relationships |>
            dplyr::pull(concept_id_2),
        concept_relationships |>
            dplyr::pull(concept_id_1)
    ) |>
        unique()

    # - Get concepts
    # Get all the concepts for the descendantConceptIds
    sql <- "SELECT * FROM @vocabularyDatabaseSchema.concept WHERE concept_id IN (@descendantConceptIds);"
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, descendantConceptIds = paste(descendantConceptIds, collapse = ","))
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    concepts <- DatabaseConnector::dbGetQuery(connection, sql) |> tibble::as_tibble()



    return(list(
        concept_relationships = concept_relationships,
        concepts = concepts,
        code_counts = code_counts
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


#' Get concept hierarchy for building a tree structure
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The root concept ID to build the hierarchy from
#'
#' @return A tibble with parent-child relationships showing the concept hierarchy
#'
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom tibble as_tibble
#' @importFrom readr read_file
#'
#' @export
getConceptHierarchy <- function(
    CDMdbHandler,
    conceptId) {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    conceptId |> checkmate::assertIntegerish(len = 1)

    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema

    #
    # FUNCTION
    #

    # Read the SQL file
    sql_file_path <- system.file("sql", "sql_server", "getConceptHierarchy.sql", package = "ROMOPAPI")
    sql <- readr::read_file(sql_file_path)

    # Render the SQL with parameters
    sql <- SqlRender::render(sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptId = conceptId
    )

    # Translate to the target database dialect
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)

    # Execute the query
    hierarchy <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble()

    return(hierarchy)
}


# Alternative function that returns hex color
single_concept_to_hex <- function(concept_id) {
  hash <- digest::digest(as.character(concept_id), algo = "md5")
  paste0("#", substr(hash, 1, 6))
}
