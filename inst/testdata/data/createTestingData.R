# Get connection
Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-DBI")
Sys.setenv(BUILD_COUNTS_TABLE = "FALSE")
source("tests/testthat/setup.R")

# List of conceptIds to extract
conceptIds <- c(
    317009, # Snomed: Asthma
    45596282, # ICD10: Asthma
    21601855, # ATC level 4: C10AA (Statins)
    320136, # Big graph, parent of Asthma snomed concept (Disorders of the respiratory system)
    4024567,# biger
    21600744 # bug in plot
)

CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
helper_createSqliteDatabaseFromDatabase(
    CDMdbHandler,
    conceptIds = conceptIds,
    pathToSqliteDatabase = "inst/testdata/data/FinnGenR13_countsOnly.sqlite"
)


# Test
connection  <- DatabaseConnector::connect(DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = "inst/testdata/data/FinnGenR13_countsOnly.sqlite"))

DatabaseConnector::dbListTables(connection) |> 
    sort() |>
    expect_equal(c("cdm_source", "code_counts", "concept", "concept_ancestor", "stratified_code_counts"))

dplyr::tbl(connection, "concept") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

dplyr::tbl(connection, "concept_ancestor") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

dplyr::tbl(connection, "code_counts") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

dplyr::tbl(connection, "stratified_code_counts") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

dplyr::tbl(connection, "cdm_source") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

