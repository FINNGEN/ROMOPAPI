#' Get visit type names
#'
#' @description
#' Retrieves a list of visit group concept IDs used in the stratified code counts table
#' along with their associated concept names and codes.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param stratifiedCodeCountsTable Name of the stratified code counts table. Defaults to "stratified_code_counts"
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item `visitGroupConceptId` - The visit group concept ID
#'   \item `conceptCode` - The OMOP concept code
#'   \item `conceptName` - The human-readable concept name
#' }
#'
#' @importFrom checkmate assertClass
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get visit type names
#' result <- getVisitTypeNames(CDMdbHandler)
#' }
getVisitTypeNames <- function(
    CDMdbHandler,
    stratifiedCodeCountsTable = "stratified_code_counts") {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")

    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    ParallelLogger::logInfo("getVisitTypeNames: Getting visit type names")

    #
    # FUNCTION
    #
    sql <- "
    SELECT DISTINCT 
        scc.visit_group_concept_id AS visitGroupConceptId,
        c.concept_code AS conceptCode,
        c.concept_name AS conceptName
    FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable scc
    INNER JOIN @vocabularyDatabaseSchema.concept c
        ON scc.visit_group_concept_id = c.concept_id
    WHERE scc.visit_group_concept_id != 0
    ORDER BY scc.visit_group_concept_id;"

    visitTypeNames <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedCodeCountsTable = stratifiedCodeCountsTable
    ) |>
        tibble::as_tibble()

    return(visitTypeNames)
}

#' Memoised version of getVisitTypeNames
#'
#' @description
#' A memoised version of the getVisitTypeNames function that caches results to improve performance
#' for repeated calls with the same parameters. The CDMdbHandler argument is omitted from
#' the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param stratifiedCodeCountsTable Name of the stratified code counts table. Defaults to "stratified_code_counts"
#'
#' @importFrom memoise memoise
#' 
#' @return A tibble with columns:
#' \itemize{
#'   \item `visitGroupConceptId` - The visit group concept ID
#'   \item `conceptCode` - The OMOP concept code
#'   \item `conceptName` - The human-readable concept name
#' }
#'
#' @export
getVisitTypeNames_memoise <- memoise::memoise(
    getVisitTypeNames,
    omit_args = "CDMdbHandler"
)
