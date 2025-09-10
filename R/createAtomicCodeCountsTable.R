#' Create atomic code counts table
#'
#' @description
#' Creates a table containing atomic-level counts of codes by concept, year, gender and age decile.
#' This function processes each domain separately and creates a comprehensive table with event counts
#' for individual concepts before aggregation.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param domains Optional data frame defining domains to process. If NULL, uses standard OMOP domains
#' @param codeAtomicCountsTable Name of the atomic counts table to create. Defaults to "code_atomic_counts"
#'
#' @return Nothing. Creates a table called 'code_atomic_counts' in the results schema with columns:
#' \itemize{
#'   \item `concept_id` - The OMOP concept ID
#'   \item `maps_to_concept_id` - The mapped concept ID
#'   \item `calendar_year` - The year of the events
#'   \item `gender_concept_id` - The gender concept ID
#'   \item `age_decile` - The age decile (0-9, 10-19, etc.)
#'   \item `event_counts` - Number of events for this code
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
#' # Create atomic code counts table for all domains
#' createAtomicCodeCountsTable(CDMdbHandler)
#' 
#' # Create atomic code counts table with custom domain configuration
#' custom_domains <- tibble::tribble(
#'   ~domain_id, ~table_name, ~concept_id_field, ~date_field, ~maps_to_concept_id_field,
#'   "Condition", "condition_occurrence", "condition_concept_id", "condition_start_date", "condition_concept_id"
#' )
#' createAtomicCodeCountsTable(CDMdbHandler, domains = custom_domains)
#' }
createAtomicCodeCountsTable <- function(
    CDMdbHandler,
    domains = NULL, 
    codeAtomicCountsTable = "code_atomic_counts") {
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
            "Condition", "condition_occurrence", "condition_source_concept_id", "condition_start_date", "condition_concept_id",
            "Procedure", "procedure_occurrence", "procedure_source_concept_id", "procedure_date", "procedure_concept_id",
            "Drug", "drug_exposure", "drug_source_concept_id", "drug_exposure_start_date", "drug_concept_id",
            "Measurement", "measurement", "measurement_source_concept_id", "measurement_date", "measurement_concept_id",
            "Observation", "observation", "observation_source_concept_id", "observation_date", "observation_concept_id",
            "Device", "device_exposure", "device_source_concept_id", "device_exposure_start_date", "device_concept_id",
            "Visit", "visit_occurrence", "visit_source_concept_id", "visit_start_date", "visit_concept_id",
        )
    }

    domains |> checkmate::assertDataFrame()
    domains |> names() |> checkmate::assertSubset(c("domain_id", "table_name", "concept_id_field", "date_field", "maps_to_concept_id_field"))

    #
    # FUNCTION
    #

    # - Create code counts table for each domain
    sqlPath <- system.file("sql", "sql_server", "appendToAtomicCodeCountsTable.sql", package = "ROMOPAPI")
    baseSql <- SqlRender::readSql(sqlPath)

    sql <- "DROP TABLE IF EXISTS @resultsDatabaseSchema.@codeAtomicCountsTable;
    CREATE TABLE @resultsDatabaseSchema.@codeAtomicCountsTable (
        concept_id INTEGER,
        maps_to_concept_id INTEGER,
        calendar_year INTEGER,
        gender_concept_id INTEGER,
        age_decile INTEGER,
        event_counts INTEGER
    )"
    sql <- SqlRender::render(sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        codeAtomicCountsTable = codeAtomicCountsTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection, sql)

    for (i in 1:nrow(domains)) {
        domain <- domains[i, ]
        message(sprintf("Processing domain: %s", domain$table_name))
        sql <- SqlRender::render(baseSql,
            codeAtomicCountsTable = codeAtomicCountsTable,
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
