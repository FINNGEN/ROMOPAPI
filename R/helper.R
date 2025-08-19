
#' Get FinnGen Eunomia database file
#'
#' @description
#' Downloads and extracts the FinnGen Eunomia database if it doesn't exist locally.
#' Copies the database to a temporary directory and returns the path. Optionally
#' creates a counts database with aggregated statistics.
#'
#' @param counts Logical. If TRUE, creates a counts database with aggregated statistics.
#'   Defaults to FALSE.
#'
#' @return Path to the FinnGen Eunomia SQLite database file (temporary copy)
#'
#' @importFrom Eunomia extractLoadData
#' @importFrom utils download.file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get basic database file
#' db_path <- helper_FinnGen_getDatabaseFile()
#' 
#' # Get database file with counts table
#' db_path_with_counts <- helper_FinnGen_getDatabaseFile(counts = TRUE)
#' }
#'
#' @note
#' This function requires the `EUNOMIA_DATA_FOLDER` environment variable to be set
#' to the path of the Eunomia data folder.
helper_FinnGen_getDatabaseFile <- function(counts = FALSE){
   if( Sys.getenv("EUNOMIA_DATA_FOLDER") == "" ){
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  urlToFinnGenEunomiaZip <- "https://raw.githubusercontent.com/FINNGEN/EunomiaDatasets/main/datasets/FinnGenR12/FinnGenR12_v5.4.zip"
  eunomiaDataFolder <- Sys.getenv("EUNOMIA_DATA_FOLDER")

  pathToDatabase <- file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite")
  pathToDatabaseCounts <- file.path(eunomiaDataFolder, "FinnGenR12_v5.4_counts.sqlite")

  # Download the database if it doesn't exist
  if (!file.exists(file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip")) |
   !file.exists(file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite")) |
   !file.exists(file.path(eunomiaDataFolder, "FinnGenR12_v5.4_counts.sqlite"))){

    result <- utils::download.file(
      url = urlToFinnGenEunomiaZip,
      destfile = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip"),
      mode = "wb"
    )

    Eunomia::extractLoadData(
      from = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip"),
      to = pathToDatabase,
      cdmVersion = '5.4',
      verbose = TRUE
    )
    
  }

  # make a copy if counts database doesn't exist
  if (!file.exists(pathToDatabaseCounts) & counts) {

    file.copy(
      from = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"),
      to = pathToDatabaseCounts,
      overwrite = TRUE
    )
    # create code counts table
    connectionList <- list(
      database = list(
        databaseId = "F1",
        databaseName = "FinnGen",
        databaseDescription = "Eunomia database FinnGen"
      ),
      connection = list(
        connectionDetailsSettings = list(
          dbms = "sqlite",
          server = pathToDatabaseCounts
        )
      ),
      cdm = list(
        cdmDatabaseSchema = "main",
        vocabularyDatabaseSchema = "main"
      ),
      cohortTable = list(
        cohortDatabaseSchema = "main",
        cohortTableName = "test_cohort_table_<timestamp>"
      )
    )
    CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(connectionList, loadConnectionChecksLevel = "basicChecks")
    createCodeCountsTable(CDMdbHandler)
    CDMdbHandler$finalize()
    
  }

  if (counts) {
    pathFromCopyDatabase <- pathToDatabaseCounts
  } else {
    pathFromCopyDatabase <- pathToDatabase
  }

  # copy to a temp folder
  pathToCopyDatabase <- file.path(tempdir(), "FinnGenR12_v5.4.sqlite")
  file.copy(
    from = pathFromCopyDatabase,
    to = pathToCopyDatabase,
    overwrite = TRUE
  )

  return(pathToCopyDatabase)
}