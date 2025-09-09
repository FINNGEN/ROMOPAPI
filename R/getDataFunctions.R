#' Get code counts and related concept information
#'
#' @description
#' Retrieves code counts and concept relationships for specified concept IDs from an OMOP CDM database.
#' This function fetches parent concepts, descendant concepts, and mapped concepts, along with
#' their associated event counts.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get counts and relationships for
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
#' result <- getCodeCounts(CDMdbHandler, conceptId = 317009)
#'
#' # View concept relationships
#' print(result$concept_relationships)
#' }
getCodeCounts <- function(
    CDMdbHandler,
    conceptId) {
    message("getCodeCounts: ", conceptId)
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    conceptId |> checkmate::assertIntegerish(lower = 1)

    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    # - Get concept parents and all descendants, only if they have code counts
    sql <- "
    WITH RECURSIVE concept_tree AS (
        SELECT
            ancestor_concept_id,
            descendant_concept_id,
            1 as level
        FROM @vocabularyDatabaseSchema.concept_ancestor
        WHERE ancestor_concept_id IN (@conceptId)
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
        ct.ancestor_concept_id as parent_concept_id,
        ct.descendant_concept_id as child_concept_id,
        ct.level
    FROM concept_tree ct
    -- Get only the descendants of the conceptId
    INNER JOIN @resultsDatabaseSchema.code_counts cc 
    ON ct.descendant_concept_id = cc.concept_id

    UNION ALL

    SELECT DISTINCT
        descendant_concept_id as parent_concept_id,
        ancestor_concept_id as child_concept_id,
        -1 as level
    FROM @vocabularyDatabaseSchema.concept_ancestor ca
    -- Get only the descendants that have code counts
    INNER JOIN @resultsDatabaseSchema.code_counts cc 
    ON ca.descendant_concept_id = cc.concept_id
    WHERE descendant_concept_id IN (@conceptId) AND min_levels_of_separation = 1
    "

    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, conceptId = conceptId, resultsDatabaseSchema = resultsDatabaseSchema)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)

    # Gets tree of descendants and the code counts for each descendant
    data <- DatabaseConnector::dbGetQuery(connection, sql) |>
        dplyr::mutate(level = as.character(level)) |>
        tibble::as_tibble()

    familyTree <-  dplyr::bind_rows(
        data  |> dplyr::filter(level != "0"), 
        tibble::tibble(
            parent_concept_id = {{conceptId}},
            child_concept_id = {{conceptId}},
            level = "0"
        )
    ) |> 
    dplyr::arrange(level)

    parentAndDescendantsConceptIds <- familyTree |>
        dplyr::pull(child_concept_id) |>
        unique()

    # - Get mapped concepts to the parent and descendants, only if they have code counts
    # Get all 'Maps to', 'Mapped from', for the family concepts
    sql <- "SELECT DISTINCT
        concept_id_1 AS parent_concept_id,
        concept_id_2 AS child_concept_id,
        relationship_id AS level
    FROM @vocabularyDatabaseSchema.concept_relationship cr
    -- Get only the descendants that have code counts
    INNER JOIN @resultsDatabaseSchema.code_counts cc
    ON cr.concept_id_2 = cc.concept_id
    --
    WHERE relationship_id IN ('Maps to','Mapped from')
    AND concept_id_1 != concept_id_2
    AND concept_id_1 IN (@parentAndDescendantsConceptIds);"
    sql <- SqlRender::render(sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema, resultsDatabaseSchema = resultsDatabaseSchema,
        parentAndDescendantsConceptIds = paste(parentAndDescendantsConceptIds, collapse = ",")
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    concept_relationships <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble()

    familyTreeWithRelationships <- dplyr::bind_rows(
        familyTree, 
        concept_relationships)

    parentAndDescendantsAndMappedConceptIds <- familyTreeWithRelationships |>
        dplyr::pull(child_concept_id) |>
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

    # TEMP: in eunomia missing concepts
    missingConcepts <- code_counts |>
        dplyr::anti_join(concepts, by = "concept_id") |>
        dplyr::distinct(concept_id) |>
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
    # END TEMP

    return(list(
        concept_relationships = familyTreeWithRelationships,
        code_counts = code_counts,
        concepts = concepts
    ))
}

#' Memoised version of getCodeCounts
#'
#' @description
#' A memoised version of the getCodeCounts function that caches results to improve performance
#' for repeated calls with the same parameters. The CDMdbHandler argument is omitted from
#' the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get counts and relationships for
#'
#' @return A list containing:
#' \itemize{
#'   \item `concept_relationships` - Tibble of concept relationships including 'Maps to', 'Mapped from', 'Parent', and descendant relationships
#'   \item `concepts` - Tibble of concept details for related concepts
#'   \item `code_counts` - Tibble of code counts from the code_counts table
#' }
#'
#' @export
getCodeCounts_memoise <- memoise::memoise(
    getCodeCounts, 
    omit_args = "CDMdbHandler"
)


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
    message("getCDMSource")
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
    SELECT DISTINCT c.concept_id, c.concept_name, c.concept_code, c.domain_id, c.vocabulary_id, c.standard_concept
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


#' Memoised version of getListOfConcepts
#'
#' @description
#' A memoised version of the getListOfConcepts function that caches results to improve performance
#' for repeated calls with the same parameters. The CDMdbHandler argument is omitted from
#' the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item `concept_id` - The OMOP concept ID
#'   \item `concept_name` - The human-readable concept name
#'   \item `concept_code` - The concept code
#'   \item `domain_id` - The domain identifier (e.g., Condition, Procedure, Drug, etc.)
#'   \item `vocabulary_id` - The vocabulary identifier (e.g., SNOMED, ICD10)
#'   \item `standard_concept` - Logical indicating if this is a standard concept
#' }
#'
#' @export
getListOfConcepts_memoise <- memoise::memoise(
    getListOfConcepts, 
    omit_args = "CDMdbHandler"
)
