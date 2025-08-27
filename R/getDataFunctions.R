#' Get code counts and related concept information
#'
#' @description
#' Retrieves code counts and concept relationships for specified concept IDs from an OMOP CDM database.
#' This function fetches parent concepts, descendant concepts, and mapped concepts, along with
#' their associated event counts.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Vector of concept IDs to get counts and relationships for
#'
#' @return A list containing:
#' \itemize{
#'   \item `concept_relationships` - Tibble of concept relationships including 'Maps to', 'Mapped from', 'Parent', and descendant relationships
#'   \item `concepts` - Tibble of concept details for related concepts
#'   \item `code_counts` - Tibble of code counts from the code_counts table
#' }
#'
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom tibble as_tibble
#' @importFrom dplyr pull bind_rows
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get code counts for specific concept IDs
#' result <- getCodeCounts(CDMdbHandler, conceptIds = c(317009))
#' 
#' # View concept relationships
#' print(result$concept_relationships)
#' }
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

    # TEMP : deduplicate the ATC codes 
    if(any(conceptIds > 2100000000)) {
        conceptIds <- conceptIds - 2100000000
    }
    # END TEMP
  
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
        tibble::as_tibble() |> 
        # Remove root connection or ATC to RxNorm ingredient
        dplyr::filter(!(relationship_id == "Root" & concept_id_1 != concept_id_2))

    # TEMP: add the Root, atm moment is missing for the non-standard concepts in then concept_ancestor table
    parentAndDescendants <- parentAndDescendants |>
        dplyr::filter(relationship_id != "Root") |>
        dplyr::bind_rows(
            tibble::tibble(
                concept_id_1 = conceptIds,
                concept_id_2 = conceptIds,
                relationship_id = "Root"
            )
        )
    # END TEMP

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
        tibble::as_tibble()

    # - Get concept details
    sql <- "SELECT * FROM @vocabularyDatabaseSchema.concept WHERE concept_id IN (@parentAndDescendantsAndMappedConceptIds);"
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, parentAndDescendantsAndMappedConceptIds = paste(parentAndDescendantsAndMappedConceptIds, collapse = ","))
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    concepts <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble()

    #TEMP: in eunomia missing concepts
    missingConcepts <- code_counts |> dplyr::anti_join(concepts, by = "concept_id") |> dplyr::distinct(concept_id) |> 
    dplyr::mutate(
        concept_name = "Missing concept Name",
        domain_id = "NA",
        vocabulary_id = "NA",
        concept_class_id = "NA",
        standard_concept = "NA",
        concept_code = "NA",
        valid_start_date = as.Date("1900-01-01"),
        valid_end_date = as.Date("1900-01-01"),
        invalid_reason = "NA"
    )
    concepts <- dplyr::bind_rows(concepts, missingConcepts)

    #END TEMP

    # TEMP : prune

    # - Take only the Root, Parent, 1-1 relationships
    concept_relationships <- concept_relationships  |> 
        dplyr::filter( relationship_id %in% c('Root', "Parent", "1-1"))

    # - Recaculate code_counts
    rootParentConceptIds <- concept_relationships |> 
        dplyr::filter(relationship_id %in% c("Root", "Parent")) |> 
        dplyr::pull(concept_id_2) |> 
        unique()

    childrenConceptIds <- concept_relationships |> 
        dplyr::filter(relationship_id == "1-1") |> 
        dplyr::pull(concept_id_2) |> 
        unique()

    sql <- "
    SELECT DISTINCT
        ca.ancestor_concept_id as concept_id_1,
        ca.descendant_concept_id as concept_id_2,
    FROM @vocabularyDatabaseSchema.concept_ancestor ca
    -- Get only the descendants that have code counts
    INNER JOIN @resultsDatabaseSchema.code_counts cc
    ON ca.descendant_concept_id = cc.concept_id
    --
    WHERE ancestor_concept_id IN (@childrenConceptIds)
    "

    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, childrenConceptIds = paste(childrenConceptIds, collapse = ","), resultsDatabaseSchema = resultsDatabaseSchema)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    childrenDescendants <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble() 

    code_counts <- childrenDescendants |> 
    dplyr::left_join(
        code_counts,
        by = c("concept_id_2" = "concept_id")
    ) |> 
    dplyr::group_by(concept_id_1, calendar_year, gender_concept_id, age_decile) |>
    dplyr::summarise(event_counts = sum(event_counts), .groups = "drop") |>
    dplyr::rename(concept_id = concept_id_1)  |> 
    dplyr::bind_rows(
        code_counts |> dplyr::filter(concept_id %in% rootParentConceptIds)
    )

    # - Filter out concepts
    concepts  <- concepts |> 
        dplyr::filter(concept_id %in% concept_relationships$concept_id_2)

    # - recode all concept id
    concept_relationships <- concept_relationships |> 
        dplyr::mutate(
            concept_id_2 = concept_id_2 + 2100000000,
            concept_id_1 = concept_id_1 + 2100000000
        )

    code_counts <- code_counts |> 
        dplyr::mutate(
            concept_id = concept_id + 2100000000
        )

    concepts <- concepts |> 
        dplyr::mutate(concept_id = concept_id + 2100000000)

    # END TEMP

    return(list(
        concept_relationships = concept_relationships,
        code_counts = code_counts,
        concepts = concepts
    ))
}


#' Get the CDM source information
#'
#' @description
#' Retrieves the CDM source information from the vocabulary schema of an OMOP CDM database.
#' This includes metadata about the database source, version, and other administrative details.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#'
#' @return A tibble containing the CDM source information with columns from the cdm_source table
#'
#' @importFrom checkmate assertClass
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom tibble as_tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get CDM source information
#' cdm_source <- getCDMSource(CDMdbHandler)
#' print(cdm_source)
#' }
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
#' @description
#' Retrieves a list of concepts that have associated code counts in the results schema.
#' This function provides concept metadata including names, vocabulary IDs, and standard concept flags
#' for concepts that are actively used in the database.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item `concept_id` - The OMOP concept ID
#'   \item `concept_name` - The human-readable concept name
#'   \item `vocabulary_id` - The vocabulary identifier (e.g., SNOMED, ICD10)
#'   \item `standard_concept` - Logical indicating if this is a standard concept
#' }
#'
#' @importFrom checkmate assertClass
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate if_else
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get list of concepts with code counts
#' concepts <- getListOfConcepts(CDMdbHandler)
#' 
#' # View concept information
#' print(concepts)
#' 
#' # Filter for standard concepts only
#' standard_concepts <- concepts[concepts$standard_concept, ]
#' }
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
    
    # TEMP : duplicate the ATC codes 
    concepts <- concepts |>
        dplyr::bind_rows(
            concepts |>
                dplyr::filter(vocabulary_id == "ATC") |>
                dplyr::mutate(
                    concept_id = concept_id + 2100000000,
                    concept_name = paste0("X ", concept_name)
                )
        )

    return(concepts)
}