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
helper_FinnGen_getDatabaseFile <- function(df = "R13", counts = FALSE) {
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  urlToFinnGenEunomiaZip <- paste0("https://raw.githubusercontent.com/FINNGEN/EunomiaDatasets/main/datasets/FinnGen", df, "/FinnGen", df, "_v5.4.zip")
  eunomiaDataFolder <- Sys.getenv("EUNOMIA_DATA_FOLDER")

  finngen.zip <- paste0("FinnGen", df, "_v5.4.zip")
  finngen.sqlite <- paste0("FinnGen", df, "_v5.4.sqlite")
  finngen.counts.sqlite <- paste0("FinnGen", df, "_v5.4_counts.sqlite")

  pathToDatabase <- file.path(eunomiaDataFolder, finngen.sqlite)
  pathToDatabaseCounts <- file.path(eunomiaDataFolder, finngen.counts.sqlite)

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

  # make a copy if counts database doesn't exist
  if (!file.exists(pathToDatabaseCounts) & counts) {
    file.copy(
      from = file.path(eunomiaDataFolder, finngen.sqlite),
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
  pathToCopyDatabase <- file.path(tempdir(), finngen.sqlite)
  file.copy(
    from = pathFromCopyDatabase,
    to = pathToCopyDatabase,
    overwrite = TRUE
  )

  return(pathToCopyDatabase)
}




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
  sql <- SqlRender::render(
    sql,
    vocabularyDatabaseSchema = sourceVocabularyDatabaseSchema,
    conceptIdsToExtract = paste(conceptIdsToExtract, collapse = ",")
  )
  sql <- SqlRender::translate(sql, targetDialect = sourceConnection@dbms)
  concept <- DatabaseConnector::dbGetQuery(sourceConnection, sql) |>
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
  sql <- SqlRender::render(
    sql,
    vocabularyDatabaseSchema = sourceVocabularyDatabaseSchema,
    conceptIdsToExtract = paste(conceptIdsToExtract, collapse = ",")
  )
  sql <- SqlRender::translate(sql, targetDialect = sourceConnection@dbms)
  conceptAncestor <- DatabaseConnector::dbGetQuery(sourceConnection, sql) |>
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
  sql <- SqlRender::render(
    sql,
    resultsDatabaseSchema = sourceResultsDatabaseSchema,
    conceptIdsToExtract = paste(conceptIdsToExtract, collapse = ","),
    codeCountsTable = codeCountsTable
  )
  sql <- SqlRender::translate(sql, targetDialect = sourceConnection@dbms)
  codeCounts <- DatabaseConnector::dbGetQuery(sourceConnection, sql) |>
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
  sql <- SqlRender::render(sql,
    resultsDatabaseSchema = sourceResultsDatabaseSchema,
    conceptIdsToExtract = paste(conceptIdsToExtract, collapse = ","),
    stratifiedCodeCountsTable = paste0("stratified_", codeCountsTable)
  )
  sql <- SqlRender::translate(sql, targetDialect = sourceConnection@dbms)
  stratifiedCodeCounts <- DatabaseConnector::dbGetQuery(sourceConnection, sql) |>
    tibble::as_tibble()

  targetConnection |> DatabaseConnector::insertTable(
    tableName = paste0("stratified_", codeCountsTable),
    data = stratifiedCodeCounts,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
  )

  # disconnect
  targetConnection |> DatabaseConnector::disconnect()
  sourceConnection |> DatabaseConnector::disconnect()
}
