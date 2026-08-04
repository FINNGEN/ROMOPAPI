#
# SELECT DATABASE AND BUILD CONFIGURATION
#
# Each testing database has one job (see AGENTS.md "Testing instructions"):
#   - Eunomia-GiBleed      sqlite,   counts-table creation only
#   - OnlyCounts-FinnGen   sqlite,   post-counts functions only (ships counts)
#   - AtlasDevelopment-5k  BigQuery, both (small subset of -full)
#   - AtlasDevelopment-full BigQuery, regenerate the OnlyCounts-FinnGen fixture

# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-GiBleed")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "OnlyCounts-FinnGen")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-5k")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-full")
testingDatabase <- Sys.getenv("HADESEXTAS_TESTING_ENVIRONMENT")
buildCountsTable <- Sys.getenv("BUILD_COUNTS_TABLE")

# check correct settings
possibleDatabases <- c("Eunomia-GiBleed", "OnlyCounts-FinnGen", "AtlasDevelopment-5k", "AtlasDevelopment-full")
if (!(testingDatabase %in% possibleDatabases)) {
  message("Please set a valid testing environment in envar HADESEXTAS_TESTING_ENVIRONMENT, from: ", paste(possibleDatabases, collapse = ", "))
  stop()
}

if (! buildCountsTable %in% c("TRUE", "FALSE")) {
  message(" BUILD_COUNTS_TABLE not found seting to FALSE")
  buildCountsTable <- "FALSE"
}

# Which databases each test stage runs against (used by skip_if_not in tests).
creationDatabases <- c("Eunomia-GiBleed", "AtlasDevelopment-5k")
postCountsDatabases <- c("OnlyCounts-FinnGen", "AtlasDevelopment-5k")

# visitSourceGroupConceptIds is database-dependent and lives in the config;
# default to 0 (grouping disabled) when a database does not set it.
test_visitSourceGroupConceptIds <- 0

#
# OnlyCounts-FinnGen — sqlite shipped with counts precomputed
#
if (testingDatabase == "OnlyCounts-FinnGen") {
  test_databasesConfig <- HadesExtras_readAndParseYaml(
    pathToYalmFile = system.file("testdata", "config", "databasesConfig.yml", package = "ROMOPAPI"),
    pathToFinnGenCountsSqlite = helper_FinnGen_getDatabaseFileCounts()
  )
  test_cohortTableHandlerConfig <- test_databasesConfig$FC$cohortTableHandler
  test_visitSourceGroupConceptIds <- test_databasesConfig$FC$visitSourceGroupConceptIds

  # ships precomputed counts, never rebuild
  buildCountsTable <- "FALSE"
}

#
# Eunomia-GiBleed — raw OMOP CDM (sqlite)
#
if (testingDatabase == "Eunomia-GiBleed") {
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  pathToGiBleedEunomiaSqlite <- Eunomia::getDatabaseFile("GiBleed", overwrite = FALSE)

  test_databasesConfig <- HadesExtras_readAndParseYaml(
    pathToYalmFile = system.file("testdata", "config", "databasesConfig.yml", package = "ROMOPAPI"),
    pathToGiBleedEunomiaSqlite = pathToGiBleedEunomiaSqlite
  )
  test_cohortTableHandlerConfig <- test_databasesConfig$E1$cohortTableHandler
  test_visitSourceGroupConceptIds <- test_databasesConfig$E1$visitSourceGroupConceptIds
}

#
# AtlasDevelopment — BigQuery (5k subset or full)
#
if (testingDatabase |> stringr::str_starts("AtlasDevelopment")) {
  if (Sys.getenv("GCP_SERVICE_KEY") == "") {
    message("GCP_SERVICE_KEY not set. Please set this environment variable to the path of the GCP service key.")
    stop()
  }

  bigrquery::bq_auth(path = Sys.getenv("GCP_SERVICE_KEY"))

  test_databasesConfig <- HadesExtras_readAndParseYaml(
    pathToYalmFile = system.file("testdata", "config", "databasesConfig.yml", package = "ROMOPAPI")
  )

  databaseEntry <- if (testingDatabase |> stringr::str_ends("5k")) {
    test_databasesConfig$BQ5K
  } else {
    test_databasesConfig$BQfull
  }
  test_cohortTableHandlerConfig <- databaseEntry$cohortTableHandler
  test_visitSourceGroupConceptIds <- databaseEntry$visitSourceGroupConceptIds
}

# guard against a missing config value
if (is.null(test_visitSourceGroupConceptIds)) {
  test_visitSourceGroupConceptIds <- 0
}

#
# INFORM USER
#
message("************* Testing on: ")
message("Database: ", testingDatabase)

if (buildCountsTable == "TRUE") {
  message("************* Building counts table")
  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  createCodeCountsTables(
    CDMdbHandler,
    codeCountsTable = "code_counts",
    visitSourceGroupConceptIds = test_visitSourceGroupConceptIds
  )
} else {
  message("************* Not building counts table")
}
