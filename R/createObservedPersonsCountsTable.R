#' Create observed persons counts table
#'
#' @description
#' Creates the population-at-risk denominator table used by
#' `getPersonCountsPrevalence()`: one row per `calendar_year x
#' gender_concept_id x age_decile`, counting every distinct person under
#' observation in that year (via `person` joined to `observation_period`),
#' independent of any concept or visit group. Must be created after
#' `createStratifiedCodeCountsTable()`, since the set of years it covers is
#' read from that table.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param observedPersonsCountsTable Name of the table to create. Defaults to
#'   "observed_persons_counts_stratified"
#' @param stratifiedCodeCountsTable Name of the stratified code counts table to read
#'   the calendar years from. Defaults to "stratified_code_counts"
#'
#' @return Nothing. Creates a table in the results schema with columns:
#' \itemize{
#'   \item `calendar_year` - The calendar year
#'   \item `gender_concept_id` - The gender concept ID
#'   \item `age_decile` - The age decile (0-9, 10-19, etc.)
#'   \item `observed_persons_counts` - Number of distinct persons under observation in that stratum
#' }
#'
#' @importFrom checkmate assertClass
#' @importFrom SqlRender readSql render translate
#' @importFrom DatabaseConnector executeSql
#'
#' @export
createObservedPersonsCountsTable <- function(
    CDMdbHandler,
    observedPersonsCountsTable = "observed_persons_counts_stratified",
    stratifiedCodeCountsTable = "stratified_code_counts") {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    connection <- CDMdbHandler$connectionHandler$getConnection()
    cdmDatabaseSchema <- CDMdbHandler$cdmDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #
    sqlDialectFolder <- if (connection@dbms == "bigquery") "bigquery" else "sql_server"
    sqlPath <- system.file(
        "sql",
        sqlDialectFolder,
        "createObservedPersonsCountsTable.sql",
        package = "ROMOPAPI"
    )
    sql <- SqlRender::readSql(sqlPath)
    sql <- SqlRender::render(
        sql,
        cdmDatabaseSchema = cdmDatabaseSchema,
        resultsDatabaseSchema = resultsDatabaseSchema,
        observedPersonsCountsTable = observedPersonsCountsTable,
        stratifiedCodeCountsTable = stratifiedCodeCountsTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection, sql)
}
