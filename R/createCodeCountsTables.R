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
#'
#' @return Nothing. Creates a table called 'code_counts' in the results schema with columns:
#' \itemize{
#'   \item `domain` - The domain of the code (Condition, Drug, etc.)
#'   \item `concept_id` - The OMOP concept ID
#'   \item `calendar_year` - The year of the events
#'   \item `gender_concept_id` - The gender concept ID
#'   \item `age_decile` - The age decile (0-9, 10-19, etc.)
#'   \item `record_counts` - Number of events for this code
#'   \item `person_counts` - Number of persons with this code
#'   \item `incidence_person_counts` - Number of persons with first occurrence of this code
#'   \item `descendant_record_counts` - Number of events including descendant concepts
#'   \item `descendant_person_counts` - Number of persons including descendant concepts
#'   \item `descendant_incidence_person_counts` - Number of persons with first occurrence including descendants
#'   \item `total_person_counts` - Total number of persons in the stratum
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
    codeCountsTable = "code_counts") {
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
    createStratifiedCodeCountsTable(CDMdbHandler, domains = domains, stratifiedCodeCountsTable = stratifiedCodeCountsTable)


    # - Create code counts table
    sqlPath <- system.file("sql", "sql_server", "createCodeCountsTable.sql", package = "ROMOPAPI")
    sql <- SqlRender::readSql(sqlPath)
    sql <- SqlRender::render(sql,
        cdmDatabaseSchema = cdmDatabaseSchema,
        resultsDatabaseSchema = resultsDatabaseSchema,
        codeCountsTable = codeCountsTable,
        stratifiedCodeCountsTable = stratifiedCodeCountsTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection, sql)

    # - delete atomic code counts table
    #CDMdbHandler$connectionHandler$executeSql(paste0("DROP TABLE ", resultsDatabaseSchema, ".", codeAtomicCountsTable))
}
