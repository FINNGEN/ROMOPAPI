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



  # Get concept ids for descendants, descendants of descendants, and parent 
  sql <- "SELECT DISTINCT ca2.descendant_concept_id AS concept_id 
          FROM @vocabularyDatabaseSchema.concept_ancestor AS ca1
          LEFT JOIN @vocabularyDatabaseSchema.concept_ancestor ca2
          ON ca1.descendant_concept_id = ca2.ancestor_concept_id
          WHERE ca1.ancestor_concept_id IN (@conceptIds)
          UNION ALL
          SELECT DISTINCT ca3.ancestor_concept_id AS concept_id 
          FROM @vocabularyDatabaseSchema.concept_ancestor AS ca3
          WHERE ca3.descendant_concept_id IN (@conceptIds) AND ca3.min_levels_of_separation = 1
          "

  sql <- SqlRender::render(sql, vocabularyDatabaseSchema = sourceVocabularyDatabaseSchema, conceptIds = paste(conceptIds, collapse = ","))
  sql <- SqlRender::translate(sql, targetDialect = sourceConnection@dbms)
  conceptIds <- DatabaseConnector::dbGetQuery(sourceConnection, sql) |>
    tibble::as_tibble() |>
    pull(concept_id)


  # Get concept table
  sql <- "SELECT DISTINCT c.* FROM @vocabularyDatabaseSchema.concept c
          WHERE c.concept_id IN (@conceptIds)"
  sql <- SqlRender::render(sql, vocabularyDatabaseSchema = sourceVocabularyDatabaseSchema, conceptIds = paste(conceptIds, collapse = ","))
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
         WHERE ca.ancestor_concept_id IN (@conceptIds) "
  sql <- SqlRender::render(sql, vocabularyDatabaseSchema = sourceVocabularyDatabaseSchema, conceptIds = paste(conceptIds, collapse = ","))
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
         WHERE cc.concept_id IN (@conceptIds)"
  sql <- SqlRender::render(sql, resultsDatabaseSchema = sourceResultsDatabaseSchema, conceptIds = paste(conceptIds, collapse = ","), codeCountsTable = codeCountsTable)
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
         WHERE cc.concept_id IN (@conceptIds)"
  sql <- SqlRender::render(sql, resultsDatabaseSchema = sourceResultsDatabaseSchema, conceptIds = paste(conceptIds, collapse = ","), 
  stratifiedCodeCountsTable = paste0("stratified_", codeCountsTable))
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
