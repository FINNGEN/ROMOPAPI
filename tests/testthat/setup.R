#
# SELECT DATABASE and CO2 CONFIGURATION
#

# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-GiBleed")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-DBI")
# Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-FinnGen")
Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-FinnGen-counts")
testingDatabase <- Sys.getenv("HADESEXTAS_TESTING_ENVIRONMENT")

# CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
# createCodeCountsTable(CDMdbHandler, codeCountsTable = "code_counts")

# check correct settings
possibleDatabases <- c( "AtlasDevelopment-DBI", "Eunomia-GiBleed", "Eunomia-FinnGen", "Eunomia-FinnGen-counts")
if (!(testingDatabase %in% possibleDatabases)) {
  message("Please set a valid testing environment in envar HADESEXTAS_TESTING_ENVIRONMENT, from: ", paste(possibleDatabases, collapse = ", "))
  stop()
}


#
# Eunomia Databases
#
if (testingDatabase |> stringr::str_starts("Eunomia")) {
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  pathToGiBleedEunomiaSqlite <- ""
  pathToMIMICEunomiaSqlite <- ""
  pathToFinnGenEunomiaSqlite <- ""
  if (testingDatabase |> stringr::str_ends("GiBleed")) {
    pathToGiBleedEunomiaSqlite <- Eunomia::getDatabaseFile("GiBleed", overwrite = FALSE)
  }
  if (testingDatabase |> stringr::str_ends("MIMIC")) {
    pathToMIMICEunomiaSqlite <- Eunomia::getDatabaseFile("MIMIC", overwrite = FALSE)
  }
  if (testingDatabase |> stringr::str_ends("FinnGen")) {
    pathToFinnGenEunomiaSqlite <- helper_FinnGen_getDatabaseFile()
  }
  if (testingDatabase |> stringr::str_ends("FinnGen-counts")) {
    pathToFinnGenEunomiaSqlite <- helper_FinnGen_getDatabaseFile(counts = TRUE)
  }

  test_databasesConfig <- HadesExtras::readAndParseYaml(
    pathToYalmFile = system.file("testdata", "config", "eunomia_databasesConfig.yml", package = "ROMOPAPI"),
    pathToGiBleedEunomiaSqlite = pathToGiBleedEunomiaSqlite,
    pathToMIMICEunomiaSqlite = pathToMIMICEunomiaSqlite,
    pathToFinnGenEunomiaSqlite = pathToFinnGenEunomiaSqlite
  )

  if (testingDatabase |> stringr::str_ends("GiBleed")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[1]]$cohortTableHandler
  }
  if (testingDatabase |> stringr::str_ends("MIMIC")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[2]]$cohortTableHandler
  }
  if (testingDatabase |> stringr::str_ends("FinnGen") | testingDatabase |> stringr::str_ends("FinnGen-counts")) {
    test_cohortTableHandlerConfig <- test_databasesConfig[[4]]$cohortTableHandler
  }

}




#
# AtlasDevelopmet-DBI Database
#
if (testingDatabase %in% c("AtlasDevelopment-DBI")) {
  if (Sys.getenv("GCP_SERVICE_KEY") == "") {
    message("GCP_SERVICE_KEY not set. Please set this environment variable to the path of the GCP service key.")
    stop()
  }

  bigrquery::bq_auth(path = Sys.getenv("GCP_SERVICE_KEY"))

  test_databasesConfig <- HadesExtras::readAndParseYaml(
    pathToYalmFile = system.file("testdata", "config", "atlasDev_databasesConfig.yml", package = "ROMOPAPI")
  )

  test_cohortTableHandlerConfig <- test_databasesConfig[[1]]$cohortTableHandler
}



#
# INFORM USER
#
message("************* Testing on: ")
message("Database: ", testingDatabase)

#  CDMdbHandler  <- HadesExtras::createCDMdbHandlerFromList(
#   test_cohortTableHandlerConfig, 
#   loadConnectionChecksLevel = "basicChecks"
# )

# createCodeCountsTable(
#   CDMdbHandler = CDMdbHandler
# )
