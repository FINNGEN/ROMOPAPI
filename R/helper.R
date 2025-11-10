#' Get FinnGen Eunomia database file
#'
#' @description
#' Downloads and extracts the FinnGen Eunomia database if it doesn't exist locally.
#' Copies the database to a temporary directory and returns the path.
#'
#' @param df Character string specifying the FinnGen dataset version. Defaults to "R13".
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
helper_FinnGen_getDatabaseFile <- function(df = "R13") {
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  urlToFinnGenEunomiaZip <- paste0("https://raw.githubusercontent.com/FINNGEN/EunomiaDatasets/main/datasets/FinnGen", df, "/FinnGen", df, "_v5.4.zip")
  eunomiaDataFolder <- Sys.getenv("EUNOMIA_DATA_FOLDER")

  finngen.zip <- paste0("FinnGen", df, "_v5.4.zip")
  finngen.sqlite <- paste0("FinnGen", df, "_v5.4.sqlite")

  pathToDatabase <- file.path(eunomiaDataFolder, finngen.sqlite)

  # Download the database if it doesn't exist
  if (!file.exists(file.path(eunomiaDataFolder, finngen.zip)) |
    !file.exists(file.path(eunomiaDataFolder, finngen.sqlite))) {
    result <- utils::download.file(
      url = urlToFinnGenEunomiaZip,
      destfile = file.path(eunomiaDataFolder, finngen.zip),
      mode = "wb"
    )

    Eunomia::extractLoadData(
      from = file.path(eunomiaDataFolder, finngen.zip),
      to = pathToDatabase,
      cdmVersion = "5.4",
      verbose = TRUE
    )
  }
  
  # copy to a temp folder
  pathToCopyDatabase <- file.path(tempdir(), finngen.sqlite)
  file.copy(
    from = pathToDatabase,
    to = pathToCopyDatabase,
    overwrite = TRUE
  )

  return(pathToCopyDatabase)
}


#' Get FinnGen counts-only database file
#'
#' @description
#' Creates a copy of the FinnGen R13 counts-only SQLite database from the package's test data.
#' This database contains pre-computed counts and aggregated statistics for testing
#' and development purposes.
#'
#' @return Path to the FinnGen R13 counts-only SQLite database file (temporary copy)
#'
#' @export
#'
helper_FinnGen_getDatabaseFileCounts <- function() {
  
  finngen.sqlite <- paste0("FinnGenR13_countsOnly.sqlite")

  pathToDatabase <- system.file("testdata", "data", finngen.sqlite, package = "ROMOPAPI")

  # copy to a temp folder
  pathToCopyDatabase <- file.path(tempdir(), finngen.sqlite)
  file.copy(
    from = pathToDatabase,
    to = pathToCopyDatabase,
    overwrite = TRUE
  )

  return(pathToCopyDatabase)
  
}




#' Create SQLite database from CDM database
#'
#' @description
#' Creates a new SQLite database by extracting specific concept data from a CDM database.
#' The function extracts concept information, concept ancestors, code counts, and
#' stratified code counts for the specified concept IDs and creates a new SQLite
#' database with this subset of data.
#'
#' @param CDMdbHandler A CDMdbHandler object containing database connection details
#' @param conceptIds Vector of concept IDs to extract. Defaults to c(317009, 21601855)
#' @param pathToSqliteDatabase Path where the new SQLite database should be created.
#'   Defaults to a temporary file with .sqlite extension
#' @param codeCountsTable Name of the code counts table in the source database.
#'   Defaults to "code_counts"
#'
#' @return No return value. Creates a new SQLite database file at the specified path.
#'
#' @importFrom checkmate assertClass assertString
#' @importFrom DatabaseConnector createConnectionDetails connect disconnect insertTable renderTranslateQuerySql
#' @importFrom tibble as_tibble
#'
#' @export
#'
helper_createSqliteDatabaseFromDatabase <- function(
    CDMdbHandler,
    conceptIds = c(317009, 21601855),
    pathToSqliteDatabase = tempfile(fileext = ".sqlite"),
    codeCountsTable = "code_counts") {
  CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
  pathToSqliteDatabase |> checkmate::assertString()

  # Source database details
  sourceConnection <- CDMdbHandler$connectionHandler$getConnection()
  sourceVocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
  sourceResultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema


  # Target database details
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = pathToSqliteDatabase
  )
  targetConnection <- DatabaseConnector::connect(connectionDetails)
  targetVocabularyDatabaseSchema <- "main"
  targetResultsDatabaseSchema <- "main"

  conceptIdsToExtract <- c()
  for (conceptId in conceptIds) {
    results <- getCodeCounts(
      CDMdbHandler,
      conceptId = conceptId
    )

    conceptIdsToExtract <- c(conceptIdsToExtract, results$concepts$concept_id)
  }

  conceptIdsToExtract <- conceptIdsToExtract |> unique()

  # Get concept table
  sql <- "SELECT DISTINCT c.* FROM @vocabularyDatabaseSchema.concept c
          WHERE c.concept_id IN (@conceptIdsToExtract)"

  concept <- DatabaseConnector::renderTranslateQuerySql(
    connection = sourceConnection,
    sql = sql,
    vocabularyDatabaseSchema = sourceVocabularyDatabaseSchema,
    conceptIdsToExtract = paste(conceptIdsToExtract, collapse = ",")
  ) |>
    tibble::as_tibble()

  targetConnection |> DatabaseConnector::insertTable(
    tableName = "concept",
    data = concept,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
  )

  # Concept ancestor table
  sql <- "SELECT DISTINCT ca.* FROM @vocabularyDatabaseSchema.concept_ancestor ca
         WHERE ca.descendant_concept_id IN (@conceptIdsToExtract) "
  conceptAncestor <- DatabaseConnector::renderTranslateQuerySql(
    connection = sourceConnection,
    sql = sql,
    vocabularyDatabaseSchema = sourceVocabularyDatabaseSchema,
    conceptIdsToExtract = paste(conceptIdsToExtract, collapse = ",")
  ) |>
    tibble::as_tibble()

  targetConnection |> DatabaseConnector::insertTable(
    tableName = "concept_ancestor",
    data = conceptAncestor,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
  )

  # Code counts table
  sql <- "SELECT DISTINCT cc.* FROM @resultsDatabaseSchema.@codeCountsTable cc
         WHERE cc.concept_id IN (@conceptIdsToExtract)"
  codeCounts <- DatabaseConnector::renderTranslateQuerySql(
    connection = sourceConnection,
    sql = sql,
    resultsDatabaseSchema = sourceResultsDatabaseSchema,
    conceptIdsToExtract = paste(conceptIdsToExtract, collapse = ","),
    codeCountsTable = codeCountsTable
  ) |>
    tibble::as_tibble()

  targetConnection |> DatabaseConnector::insertTable(
    tableName = codeCountsTable,
    data = codeCounts,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
  )

  # stratified code counts table
  sql <- "SELECT DISTINCT cc.* FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable cc
         WHERE cc.concept_id IN (@conceptIdsToExtract) OR cc.maps_to_concept_id IN (@conceptIdsToExtract)"
  stratifiedCodeCounts <- DatabaseConnector::renderTranslateQuerySql(
    connection = sourceConnection,
    sql = sql,
    resultsDatabaseSchema = sourceResultsDatabaseSchema,
    conceptIdsToExtract = paste(conceptIdsToExtract, collapse = ","),
    stratifiedCodeCountsTable = paste0("stratified_", codeCountsTable)
  ) |>
    tibble::as_tibble()

  targetConnection |> DatabaseConnector::insertTable(
    tableName = paste0("stratified_", codeCountsTable),
    data = stratifiedCodeCounts,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
  )

  # get CDMsource 
  sql <- "SELECT DISTINCT c.* FROM @vocabularyDatabaseSchema.cdm_source c"
  cdmSource <- DatabaseConnector::renderTranslateQuerySql(
    connection = sourceConnection,
    sql = sql,
    vocabularyDatabaseSchema = sourceVocabularyDatabaseSchema
  ) |>
    tibble::as_tibble()

  targetConnection |> DatabaseConnector::insertTable(
    tableName = "cdm_source",
    data = cdmSource,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
  )

  # disconnect
  targetConnection |> DatabaseConnector::disconnect()
  sourceConnection |> DatabaseConnector::disconnect()
}
