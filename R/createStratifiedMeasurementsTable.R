#' Create stratified measurements table
#'
#' @description
#' Creates an event-level table of measured values — one row per event that
#' carries a numeric `value_as_number` — used to compute value histograms at
#' request time (see `getMeasurementValueHistogram()`). Unlike
#' `createStratifiedPersonsTable()` this table is deliberately **not**
#' de-duplicated: a histogram counts events, so repeated identical values must
#' each keep their own row.
#'
#' The value is kept raw rather than pre-binned because the histogram's bin
#' breaks depend on the set of concepts actually queried (a descendant-expanded
#' token pools an arbitrary ancestor's descendants), which is only known at
#' request time.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param domains Optional data frame defining domains to process. If NULL, uses the
#'   Measurement domain only
#' @param stratifiedMeasurementsTable Name of the table to create. Defaults to
#'   "stratified_measurements"
#' @param visitSourceGroupConceptIds Optional vector of visit source group concept IDs to
#'   group visits by. Defaults to 0 (grouping disabled)
#'
#' @return Nothing. Creates a table called 'stratified_measurements' in the results schema
#' with columns:
#' \itemize{
#'   \item `concept_id` - The OMOP concept ID
#'   \item `maps_to_concept_id` - The mapped (source) concept ID
#'   \item `visit_group_concept_id` - The FinnGen visit-source group concept ID
#'   \item `calendar_year` - The year of the event
#'   \item `gender_concept_id` - The gender concept ID
#'   \item `age_decile` - The age decile (0-9, 10-19, etc.)
#'   \item `unit_concept_id` - The unit the value was recorded in (0 when absent)
#'   \item `value_as_number` - The measured value
#' }
#'
#' @importFrom checkmate assertClass assertDataFrame assertSubset
#' @importFrom SqlRender readSql render translate
#' @importFrom DatabaseConnector executeSql
#' @importFrom tibble tribble
#'
#' @export
createStratifiedMeasurementsTable <- function(
    CDMdbHandler,
    domains = NULL,
    stratifiedMeasurementsTable = "stratified_measurements",
    visitSourceGroupConceptIds = 0) {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    connection <- CDMdbHandler$connectionHandler$getConnection()
    cdmDatabaseSchema <- CDMdbHandler$cdmDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    # Measurement only. The template is parameterised like its two siblings so that
    # adding Observation (which also has value_as_number/unit_concept_id in CDM 5.4)
    # is a one-row change here rather than a new SQL file.
    if (is.null(domains)) {
        domains <- tibble::tribble(
            ~domain_id, ~table_name, ~concept_id_field, ~date_field, ~maps_to_concept_id_field,
            "Measurement", "measurement", "measurement_concept_id", "measurement_date", "measurement_source_concept_id"
        )
    }

    domains |> checkmate::assertDataFrame()
    domains |>
        names() |>
        checkmate::assertSubset(c("domain_id", "table_name", "concept_id_field", "date_field", "maps_to_concept_id_field"))

    #
    # FUNCTION
    #

    # The sql_server template is canonical and SqlRender translates it cleanly for every
    # backend here (verified for the window functions, FLOOR and CAST this query uses), so
    # no bigquery override is shipped. Prefer one if it ever appears, per CLAUDE.md.
    sqlPath <- ""
    if (connection@dbms == "bigquery") {
        sqlPath <- system.file("sql", "bigquery", "appendToStratifiedMeasurementsTable.sql", package = "ROMOPAPI")
    }
    if (!nzchar(sqlPath)) {
        sqlPath <- system.file("sql", "sql_server", "appendToStratifiedMeasurementsTable.sql", package = "ROMOPAPI")
    }
    baseSql <- SqlRender::readSql(sqlPath)

    sql <- "DROP TABLE IF EXISTS @resultsDatabaseSchema.@stratifiedMeasurementsTable;
    CREATE TABLE @resultsDatabaseSchema.@stratifiedMeasurementsTable (
        concept_id INTEGER,
        maps_to_concept_id INTEGER,
        visit_group_concept_id INTEGER,
        calendar_year INTEGER,
        gender_concept_id INTEGER,
        age_decile INTEGER,
        unit_concept_id INTEGER,
        value_as_number FLOAT
    )"
    sql <- SqlRender::render(sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedMeasurementsTable = stratifiedMeasurementsTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection, sql)

    for (i in 1:nrow(domains)) {
        domain <- domains[i, ]
        message(sprintf("Processing domain: %s", domain$table_name))
        sql <- SqlRender::render(baseSql,
            stratifiedMeasurementsTable = stratifiedMeasurementsTable,
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
