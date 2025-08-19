#' Create code counts table
#'
#' Creates a table containing counts of codes by domain, concept, year, gender and age decile.
#' The table includes both event counts and person counts, as well as descendant counts.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#'
#' @return Nothing. Creates a table called 'code_counts' in the results schema with columns:
#' \itemize{
#'   \item domain - The domain of the code (Condition, Drug, etc.)
#'   \item concept_id - The OMOP concept ID
#'   \item calendar_year - The year of the events
#'   \item gender_concept_id - The gender concept ID
#'   \item age_decile - The age decile (0-9, 10-19, etc.)
#'   \item event_counts - Number of events for this code
#'   \item person_counts - Number of persons with this code
#'   \item incidence_person_counts - Number of persons with first occurrence of this code
#'   \item descendant_event_counts - Number of events including descendant concepts
#'   \item descendant_person_counts - Number of persons including descendant concepts
#'   \item descendant_incidence_person_counts - Number of persons with first occurrence including descendants
#'   \item total_person_counts - Total number of persons in the stratum
#' }
#'
#' @export
createCodeCountsTable <- function(
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

    if (is.null(domains)) {
        domains <- tibble::tribble(
            ~domain_id, ~table_name, ~concept_id_field, ~date_field, ~maps_to_concept_id_field,
            "Condition", "condition_occurrence", "condition_concept_id", "condition_start_date", "condition_concept_id",
            "Procedure", "procedure_occurrence", "procedure_concept_id", "procedure_date", "procedure_concept_id",
            "Drug", "drug_exposure", "drug_concept_id", "drug_exposure_start_date", "drug_concept_id",
            "Measurement", "measurement", "measurement_concept_id", "measurement_date", "measurement_concept_id",
            "Observation", "observation", "observation_concept_id", "observation_date", "observation_concept_id",
            "Device", "device_exposure", "device_concept_id", "device_exposure_start_date", "device_concept_id",
            "Visit", "visit_occurrence", "visit_concept_id", "visit_start_date", "visit_concept_id",
            # non standard
            "Condition", "condition_occurrence", "condition_source_concept_id", "condition_start_date", "condition_source_concept_id",
            "Procedure", "procedure_occurrence", "procedure_source_concept_id", "procedure_date", "procedure_source_concept_id",
            "Drug", "drug_exposure", "drug_source_concept_id", "drug_exposure_start_date", "drug_source_concept_id",
            "Measurement", "measurement", "measurement_source_concept_id", "measurement_date", "measurement_source_concept_id",
            "Observation", "observation", "observation_source_concept_id", "observation_date", "observation_source_concept_id",
            "Device", "device_exposure", "device_source_concept_id", "device_exposure_start_date", "device_source_concept_id",
            "Visit", "visit_occurrence", "visit_source_concept_id", "visit_start_date", "visit_source_concept_id",
        )
    }

    domains |> checkmate::assertDataFrame()
    domains |> names() |> checkmate::assertSubset(c("domain_id", "table_name", "concept_id_field", "date_field", "maps_to_concept_id_field"))

    #
    # FUNCTION
    #

    # - Create observation counts table

    sqlPath <- system.file("sql", "sql_server", "createObservationCountsTable.sql", package = "ROMOPAPI")
    sql <- SqlRender::readSql(sqlPath)
    sql <- SqlRender::render(sql,
        cdmDatabaseSchema = cdmDatabaseSchema,
        resultsDatabaseSchema = resultsDatabaseSchema
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)

    DatabaseConnector::executeSql(connection, sql)

    # - Create code counts table for each domain
    sqlPath <- system.file("sql", "sql_server", "appendToCodeCountsTable.sql", package = "ROMOPAPI")
    baseSql <- SqlRender::readSql(sqlPath)

    sql <- "DROP TABLE IF EXISTS @resultsDatabaseSchema.@codeCountsTable;
    CREATE TABLE @resultsDatabaseSchema.@codeCountsTable (
        domain VARCHAR(255),
        concept_id INTEGER,
        maps_to_concept_id INTEGER,
        calendar_year INTEGER,
        gender_concept_id INTEGER,
        age_decile INTEGER,
        event_counts INTEGER,
        person_counts INTEGER,
        incidence_person_counts INTEGER,
        descendant_event_counts INTEGER,
        descendant_person_counts INTEGER,
        descendant_incidence_person_counts INTEGER,
        total_person_counts INTEGER
    )"
    sql <- SqlRender::render(sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        codeCountsTable = codeCountsTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection, sql)

    for (i in 1:nrow(domains)) {
        domain <- domains[i, ]
        message(sprintf("Processing domain: %s", domain$table_name))
        sql <- SqlRender::render(baseSql,
            codeCountsTable = codeCountsTable,
            domain_id = domain$domain_id,
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
