# Get connection
Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-DBI")
Sys.setenv(BUILD_COUNTS_TABLE = "FALSE")
source("tests/testthat/setup.R")

# List of conceptIds to extract
conceptIds <- c(
    317009, # Snomed: Asthma
    45596282, # ICD10: Asthma
    21601855, # ATC level 4: C10AA (Statins)
    320136 # Disorder of respiratory …# big 
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
    expect_equal(c("code_counts", "concept", "concept_ancestor", "stratified_code_counts"))

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


# 
Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "OnlyCounts-FinnGen")
source("tests/testthat/setup.R")

CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")

conceptId <- 21601855

report.html <- createReport(conceptId, CDMdbHandler, showsMappings = FALSE)
browseURL(report.html)
