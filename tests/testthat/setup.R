#
# SELECT DATABASE and CO2 CONFIGURATION
#

# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-GiBleed")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-MIMIC")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-FinnGen")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-5k")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-full")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "OnlyCounts-FinnGen")
testingDatabase <- Sys.getenv("HADESEXTAS_TESTING_ENVIRONMENT")
buildCountsTable <- Sys.getenv("BUILD_COUNTS_TABLE")

# check correct settings
possibleDatabases <- c("Eunomia-GiBleed", "Eunomia-MIMIC", "Eunomia-FinnGen", "AtlasDevelopment-5k", "AtlasDevelopment-full", "OnlyCounts-FinnGen")
if (!(testingDatabase %in% possibleDatabases)) {
  message("Please set a valid testing environment in envar HADESEXTAS_TESTING_ENVIRONMENT, from: ", paste(possibleDatabases, collapse = ", "))
  stop()
}

if (! buildCountsTable %in% c("TRUE", "FALSE")) {
  message(" BUILD_COUNTS_TABLE not found seting to FALSE")
  buildCountsTable <- "FALSE"
}

#
# Package testing database with only the needed tables
#
if (testingDatabase |> stringr::str_starts("OnlyCounts-FinnGen")) {
  test_databasesConfig <- HadesExtras_readAndParseYaml(
    pathToYalmFile = system.file("testdata", "config", "databasesConfig.yml", package = "ROMOPAPI"), 
    pathToFinnGenCountsSqlite = helper_FinnGen_getDatabaseFileCounts()
  )
  test_cohortTableHandlerConfig <- test_databasesConfig$FC$cohortTableHandler

  buildCountsTable <- "FALSE"
}

#
# Eunomia Databases
#
if (testingDatabase |> stringr::str_starts("Eunomia")) {
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  pathToGiBleedEunomiaSqlite <- Eunomia::getDatabaseFile("GiBleed", overwrite = FALSE)
  pathToMIMICEunomiaSqlite <- Eunomia::getDatabaseFile("MIMIC", overwrite = FALSE)

  test_databasesConfig <- HadesExtras_readAndParseYaml(
    pathToYalmFile = system.file("testdata", "config", "databasesConfig.yml", package = "ROMOPAPI"),
    pathToGiBleedEunomiaSqlite = pathToGiBleedEunomiaSqlite,
    pathToMIMICEunomiaSqlite = pathToMIMICEunomiaSqlite,
    pathToFinnGenEunomiaSqlite = helper_FinnGen_getDatabaseFile()
  )

  if (testingDatabase |> stringr::str_ends("GiBleed")) {
    test_cohortTableHandlerConfig <- test_databasesConfig$E1$cohortTableHandler
  }
  if (testingDatabase |> stringr::str_ends("MIMIC")) {
    test_cohortTableHandlerConfig <- test_databasesConfig$E2$cohortTableHandler
  }
  if (testingDatabase |> stringr::str_ends("FinnGen")) {
    test_cohortTableHandlerConfig <- test_databasesConfig$E3$cohortTableHandler
  }
  if (testingDatabase |> stringr::str_ends("FinnGen")) {
    test_cohortTableHandlerConfig <- test_databasesConfig$E4$cohortTableHandler
  }
}


#
# AtlasDevelopmet-DBI Database
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

  if (testingDatabase |> stringr::str_ends("5k")) {
    test_cohortTableHandlerConfig <- test_databasesConfig$BQ5K$cohortTableHandler
  }
  if (testingDatabase |> stringr::str_ends("full")) {
    test_cohortTableHandlerConfig <- test_databasesConfig$BQfull$cohortTableHandler
  }
}


#
# INFORM USER
#
message("************* Testing on: ")
message("Database: ", testingDatabase)

if (buildCountsTable == "TRUE") {
  message("************* Building counts table")
  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  createCodeCountsTables(CDMdbHandler, codeCountsTable = "code_counts")
}else{
  message("************* Not building counts table")
}


