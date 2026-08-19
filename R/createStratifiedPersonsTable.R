#' Create stratified persons table
#'
#' @description
#' Creates a person-level bridge table — one row per distinct
#' `concept_id x stratum x person_id` — used to compute exact distinct-person
#' counts (and, at request time, exact set overlaps) without sketches. This
#' function processes each domain separately, mirroring
#' `createStratifiedCodeCountsTable()`.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param domains Optional data frame defining domains to process. If NULL, uses standard OMOP domains
#' @param stratifiedPersonsTable Name of the stratified persons table to create. Defaults to "stratified_persons"
#' @param visitSourceGroupConceptIds Optional vector of visit source group concept IDs to filter by. Defaults to 0
#'
#' @return Nothing. Creates a table called 'stratified_persons' in the results schema with columns:
#' \itemize{
#'   \item `person_id` - The OMOP person ID
#'   \item `concept_id` - The OMOP concept ID
#'   \item `maps_to_concept_id` - The mapped concept ID
#'   \item `visit_group_concept_id` - The FinnGen visit-source group concept ID
#'   \item `calendar_year` - The year of the events
#'   \item `gender_concept_id` - The gender concept ID
#'   \item `age_decile` - The age decile (0-9, 10-19, etc.)
#' }
#'
#' @importFrom checkmate assertClass assertDataFrame assertSubset
#' @importFrom SqlRender readSql render translate
#' @importFrom DatabaseConnector executeSql
#' @importFrom tibble tribble
#'
#' @export
createStratifiedPersonsTable <- function(
    CDMdbHandler,
    domains = NULL,
    stratifiedPersonsTable = "stratified_persons",
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

    # - Create stratified persons table for each domain
    sqlDialectFolder <- if (connection@dbms == "bigquery") "bigquery" else "sql_server"
    sqlPath <- system.file("sql", sqlDialectFolder, "appendToStratifiedPersonsTable.sql", package = "ROMOPAPI")
    baseSql <- SqlRender::readSql(sqlPath)

    sql <- "DROP TABLE IF EXISTS @resultsDatabaseSchema.@stratifiedPersonsTable;
    CREATE TABLE @resultsDatabaseSchema.@stratifiedPersonsTable (
        person_id INTEGER,
        concept_id INTEGER,
        maps_to_concept_id INTEGER,
        visit_group_concept_id INTEGER,
        calendar_year INTEGER,
        gender_concept_id INTEGER,
        age_decile INTEGER
    )"
    sql <- SqlRender::render(sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedPersonsTable = stratifiedPersonsTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection, sql)

    for (i in 1:nrow(domains)) {
        domain <- domains[i, ]
        message(sprintf("Processing domain: %s", domain$table_name))
        sql <- SqlRender::render(baseSql,
            stratifiedPersonsTable = stratifiedPersonsTable,
            cdmDatabaseSchema = cdmDatabaseSchema,
            resultsDatabaseSchema = resultsDatabaseSchema,
            table_name = domain$table_name,
            concept_id_field = domain$concept_id_field,
            date_field = domain$date_field,
            maps_to_concept_id_field = domain$maps_to_concept_id_field,
            visit_group_concept_ids = paste0(visitSourceGroupConceptIds, collapse = ", ")
        )

        sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
        DatabaseConnector::executeSql(connection, sql)
    }
}
