
#' Get list of concepts with code counts
#'
#' @description
#' Retrieves a list of concepts that have associated code counts in the results schema.
#' This function provides concept metadata including names, vocabulary IDs, and standard concept flags
#' for concepts that are actively used in the database.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
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
getConceptsWithCodeCounts <- function(
    CDMdbHandler,
    codeCountsTable = "code_counts") {
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
    SELECT DISTINCT 
        c.concept_id, 
        c.concept_name, c.domain_id, c.vocabulary_id, c.concept_class_id, c.standard_concept, c.concept_code,
        cc.record_counts, cc.descendant_record_counts
       FROM @vocabularyDatabaseSchema.concept c
       INNER JOIN @resultsDatabaseSchema.@codeCountsTable cc
       ON c.concept_id = cc.concept_id;"
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, resultsDatabaseSchema = resultsDatabaseSchema, codeCountsTable = codeCountsTable)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    concepts <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble() |>
        dplyr::mutate(standard_concept = dplyr::if_else(is.na(standard_concept), TRUE, FALSE))

    return(concepts)
}

#' Memoised version of getConceptsWithCodeCounts
#'
#' @description
#' A memoised version of the getConceptsWithCodeCounts function that caches results to improve performance
#' for repeated calls with the same parameters. The CDMdbHandler argument is omitted from
#' the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @importFrom memoise memoise
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
getConceptsWithCodeCounts_memoise <- memoise::memoise(
    getConceptsWithCodeCounts,
    omit_args = "CDMdbHandler"
)
