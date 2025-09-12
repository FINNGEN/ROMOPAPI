#' Create stratified code counts table
#'
#' @description
#' Creates a table containing event-level counts of codes by concept, year, gender and age decile.
#' This function processes each domain separately and creates a comprehensive table with event counts
#' for individual concepts before aggregation.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param domains Optional data frame defining domains to process. If NULL, uses standard OMOP domains
#' @param stratifiedCodeCountsTable Name of the stratified counts table to create. Defaults to "stratified_code_counts"
#'
#' @return Nothing. Creates a table called 'stratified_code_counts' in the results schema with columns:
#' \itemize{
#'   \item `concept_id` - The OMOP concept ID
#'   \item `maps_to_concept_id` - The mapped concept ID
#'   \item `calendar_year` - The year of the events
#'   \item `gender_concept_id` - The gender concept ID
#'   \item `age_decile` - The age decile (0-9, 10-19, etc.)
#'   \item `record_counts` - Number of events for this code
#' }
#'
#' @importFrom checkmate assertClass assertDataFrame assertSubset
#' @importFrom SqlRender readSql render translate
#' @importFrom DatabaseConnector executeSql
#' @importFrom tibble tribble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create stratified code counts table for all domains
#' createStratifiedCodeCountsTable(CDMdbHandler)
#' 
#' # Create stratified code counts table with custom domain configuration
#' custom_domains <- tibble::tribble(
#'   ~domain_id, ~table_name, ~concept_id_field, ~date_field, ~maps_to_concept_id_field,
#'   "Condition", "condition_occurrence", "condition_concept_id", "condition_start_date", "condition_concept_id"
#' )
#' createStratifiedCodeCountsTable(CDMdbHandler, domains = custom_domains)
#' }
createStratifiedCodeCountsTable <- function(
    CDMdbHandler,
    domains = NULL, 
    stratifiedCodeCountsTable = "stratified_code_counts") {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    cdmDatabaseSchema <- CDMdbHandler$cdmDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    if (is.null(domains)) {
        domains <- tibble::tribble(
            ~domain_id, ~table_name, ~concept_id_field, ~date_field, ~maps_to_concept_id_field,
            # non standard
            "Condition", "condition_occurrence", "condition_concept_id", "condition_start_date", "condition_source_concept_id",
            "Procedure", "procedure_occurrence", "procedure_concept_id", "procedure_date", "procedure_source_concept_id",
            "Drug", "drug_exposure", "drug_concept_id", "drug_exposure_start_date", "drug_source_concept_id",
            "Measurement", "measurement", "measurement_concept_id", "measurement_date", "measurement_source_concept_id",
            "Observation", "observation", "observation_concept_id", "observation_date", "observation_source_concept_id",
            "Device", "device_exposure", "device_concept_id", "device_exposure_start_date", "device_source_concept_id",
            "Visit", "visit_occurrence", "visit_concept_id", "visit_start_date", "visit_source_concept_id"
        )
    }

    domains |> checkmate::assertDataFrame()
    domains |> names() |> checkmate::assertSubset(c("domain_id", "table_name", "concept_id_field", "date_field", "maps_to_concept_id_field"))

    #
    # FUNCTION
    #

    # - Create code counts table for each domain
    sqlPath <- system.file("sql", "sql_server", "appendToStratrifiedCodeCountsTable.sql", package = "ROMOPAPI")
    baseSql <- SqlRender::readSql(sqlPath)

    sql <- "DROP TABLE IF EXISTS @resultsDatabaseSchema.@stratifiedCodeCountsTable;
    CREATE TABLE @resultsDatabaseSchema.@stratifiedCodeCountsTable (
        concept_id INTEGER,
        maps_to_concept_id INTEGER,
        calendar_year INTEGER,
        gender_concept_id INTEGER,
        age_decile INTEGER,
        record_counts INTEGER
    )"
    sql <- SqlRender::render(sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedCodeCountsTable = stratifiedCodeCountsTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection, sql)

    for (i in 1:nrow(domains)) {
        domain <- domains[i, ]
        message(sprintf("Processing domain: %s", domain$table_name))
        sql <- SqlRender::render(baseSql,
            stratifiedCodeCountsTable = stratifiedCodeCountsTable,
            cdmDatabaseSchema = cdmDatabaseSchema,
            resultsDatabaseSchema = resultsDatabaseSchema,
            table_name = domain$table_name,
            concept_id_field = domain$concept_id_field,
            date_field = domain$date_field,
            maps_to_concept_id_field = domain$maps_to_concept_id_field
        )

        sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
        DatabaseConnector::executeSql(connection, sql)
    }
}
