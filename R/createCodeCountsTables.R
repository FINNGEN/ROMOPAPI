#' Create code counts table
#'
#' @description
#' Creates a comprehensive table containing counts of codes by domain, concept, year, gender and age decile.
#' The table includes both event counts and person counts, as well as descendant counts for hierarchical
#' concept analysis.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param domains Optional vector of domains to process. If NULL, processes all standard domains
#' @param codeCountsTable Name of the table to create. Defaults to "code_counts"
#' @param visitSourceGroupConceptIds Optional vector of visit source group concept IDs to filter by. Defaults to 0
#'
#' @return Nothing. Creates a table called 'code_counts' in the results schema with columns:
#' \itemize{
#'   \item `concept_id` - The OMOP concept ID
#'   \item `record_counts` - Number of events for this code
#'   \item `descendant_record_counts` - Number of events including descendant concepts
#'   \item `number_of_descendants` - Number of descendant concepts (including itself)
#'   \item `person_counts` - Number of distinct persons with this code
#'   \item `descendant_person_counts` - Number of distinct persons including descendant concepts
#' }
#'
#' @importFrom checkmate assertClass
#' @importFrom SqlRender readSql render translate
#' @importFrom DatabaseConnector executeSql
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create code counts table for all domains
#' createCodeCountsTable(CDMdbHandler)
#'
#' # Create code counts table for specific domains only
#' createCodeCountsTable(CDMdbHandler, domains = c("Condition", "Drug"))
#' }
createCodeCountsTables <- function(
    CDMdbHandler,
    domains = NULL,
    codeCountsTable = "code_counts",
    visitSourceGroupConceptIds = 0
) {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    cdmDatabaseSchema <- CDMdbHandler$cdmDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    # - Create stratified code counts table
    stratifiedCodeCountsTable <- paste0("stratified_", codeCountsTable)
    createStratifiedCodeCountsTable(
        CDMdbHandler,
        domains = domains,
        stratifiedCodeCountsTable = stratifiedCodeCountsTable,
        visitSourceGroupConceptIds = visitSourceGroupConceptIds
    )

    # - Create stratified persons bridge table
    stratifiedPersonsTable <- "stratified_persons"
    createStratifiedPersonsTable(
        CDMdbHandler,
        domains = domains,
        stratifiedPersonsTable = stratifiedPersonsTable,
        visitSourceGroupConceptIds = visitSourceGroupConceptIds
    )

    # - Create code counts table
    sqlDialectFolder <- if (connection@dbms == "bigquery") "bigquery" else "sql_server"
    sqlPath <- system.file(
        "sql",
        sqlDialectFolder,
        "createCodeCountsTable.sql",
        package = "ROMOPAPI"
    )
    sql <- SqlRender::readSql(sqlPath)
    sql <- SqlRender::render(
        sql,
        cdmDatabaseSchema = cdmDatabaseSchema,
        resultsDatabaseSchema = resultsDatabaseSchema,
        codeCountsTable = codeCountsTable,
        stratifiedCodeCountsTable = stratifiedCodeCountsTable,
        stratifiedPersonsTable = stratifiedPersonsTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection, sql)

    # - delete atomic code counts table
    #CDMdbHandler$connectionHandler$executeSql(paste0("DROP TABLE ", resultsDatabaseSchema, ".", codeAtomicCountsTable))
}
