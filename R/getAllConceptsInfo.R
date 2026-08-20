
#' Get metadata for all concepts that have code counts
#'
#' @description
#' Retrieves concept metadata (names, vocabulary IDs, standard concept flags) for every
#' concept that has associated code counts in the results schema. This is the catalogue
#' of concepts a client can search/pick from — counts themselves are not included; read
#' those from `code_counts` directly (e.g. via `getConceptRelationships()`).
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item `concept_id` - The OMOP concept ID
#'   \item `concept_name` - The human-readable concept name
#'   \item `domain_id` - The domain identifier (e.g., Condition, Procedure, Drug, etc.)
#'   \item `vocabulary_id` - The vocabulary identifier (e.g., SNOMED, ICD10)
#'   \item `concept_class_id` - The concept class
#'   \item `standard_concept` - Logical indicating if this is a standard concept
#'   \item `concept_code` - The concept code
#' }
#'
#' @importFrom checkmate assertClass
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate if_else
#'
#' @export
#'
getAllConceptsInfo <- function(
    CDMdbHandler,
    codeCountsTable = "code_counts") {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")

    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    ParallelLogger::logInfo("getAllConceptsInfo: Getting info for all concepts with code counts")

    #
    # FUNCTION
    #
    # Get concept_id, concept_name, vocabulary_id, standard_concept for all concept_ids present in code_counts
    # in one SQL call. The join to code_counts only restricts the catalogue to concepts that have counts;
    # the count columns themselves are not selected here.
    sql <- "
    SELECT DISTINCT
        c.concept_id,
        c.concept_name, c.domain_id, c.vocabulary_id, c.concept_class_id, c.standard_concept, c.concept_code
       FROM @vocabularyDatabaseSchema.concept c
       INNER JOIN @resultsDatabaseSchema.@codeCountsTable cc
       ON c.concept_id = cc.concept_id;"
    concepts <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        resultsDatabaseSchema = resultsDatabaseSchema,
        codeCountsTable = codeCountsTable
    ) |>
        tibble::as_tibble() |>
        dplyr::mutate(standard_concept = dplyr::if_else(is.na(standard_concept), FALSE, TRUE)) |>
        dplyr::mutate(concept_id = as.double(concept_id))

    return(concepts)
}

#' Memoised version of getAllConceptsInfo
#'
#' @description
#' A memoised version of the getAllConceptsInfo function that caches results to improve performance
#' for repeated calls with the same parameters. The CDMdbHandler argument is omitted from
#' the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @importFrom memoise memoise
#'
#' @return Same columns as \code{\link{getAllConceptsInfo}}.
#'
#' @export
getAllConceptsInfo_memoise <- memoise::memoise(
    getAllConceptsInfo,
    omit_args = "CDMdbHandler"
)
