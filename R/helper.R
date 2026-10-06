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
#' The function extracts concept information, concept ancestors, code counts, stratified
#' code counts, the stratified persons bridge table, and the observed persons counts
#' (prevalence denominator) table for the specified concept IDs and creates a new SQLite
#' database with this subset of data.
#'
#' @param CDMdbHandler A CDMdbHandler object containing database connection details
#' @param conceptIds Vector of concept IDs to extract. Defaults to c(317009, 21601855)
#' @param personBridgeConceptIds Subset of `conceptIds` whose trees are extracted into the
#'   `stratified_persons` bridge table. Defaults to `conceptIds` (all of them). A person-level
#'   bridge fans out to one row per person per stratum, so a broad concept's tree can make the
#'   extract far larger than the record-level tables — narrow this to keep the fixture small.
#' @param maxPersonsPerConcept Maximum distinct persons kept per target concept (in
#'   `personBridgeConceptIds`'s trees) in the `stratified_persons` extract — the lowest-`person_id`
#'   ones, deterministically. All of a kept person's stratum rows for that concept are included, so
#'   this bounds the person-level fan-out for common real-world codes without truncating any one
#'   person's detail. Defaults to 1000.
#' @param maxEventsPerConcept Maximum measurement events kept per target concept and unit in the
#'   `stratified_measurements` extract. The sample is a deterministic *systematic* one — the rows
#'   are ranked by `value_as_number`, split into that many equal-count buckets with `NTILE`, and
#'   one row is kept from each — so the sampled values keep the shape of the original distribution.
#'   Taking the first N by value instead would keep only the smallest values and destroy the very
#'   histogram this fixture exists to test. Defaults to 2000.
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
    personBridgeConceptIds = conceptIds,
    maxPersonsPerConcept = 1000,
    maxEventsPerConcept = 2000,
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
  personConceptIdsToExtract <- c()
  for (conceptId in conceptIds) {
    relationships <- getConceptRelationships(
      CDMdbHandler,
      conceptId = conceptId
    )

    conceptIdsToExtract <- c(conceptIdsToExtract, relationships$concepts$concept_id)
    if (conceptId %in% personBridgeConceptIds) {
      personConceptIdsToExtract <- c(personConceptIdsToExtract, relationships$concepts$concept_id)
    }
  }

  conceptIdsToExtract <- conceptIdsToExtract |> unique()
  personConceptIdsToExtract <- personConceptIdsToExtract |> unique()

  # Get concept table
  sql <- "SELECT DISTINCT c.* FROM @vocabularyDatabaseSchema.concept c
          WHERE c.concept_id IN (@conceptIdsToExtract)
          -- Include also the concepts in visit_group_concept_id in stratified_code_counts table
          UNION
          SELECT DISTINCT c.* FROM @vocabularyDatabaseSchema.concept c
          JOIN @resultsDatabaseSchema.@stratifiedCodeCountsTable scc ON c.concept_id = scc.visit_group_concept_id
          WHERE scc.concept_id IN (@conceptIdsToExtract) OR scc.maps_to_concept_id IN (@conceptIdsToExtract)
          -- Include also the unit concepts in stratified_measurements, otherwise
          -- getMeasurementValueHistogram() cannot resolve a unit to its concept_code
          UNION
          SELECT DISTINCT c.* FROM @vocabularyDatabaseSchema.concept c
          JOIN @resultsDatabaseSchema.stratified_measurements sm ON c.concept_id = sm.unit_concept_id
          WHERE sm.concept_id IN (@conceptIdsToExtract) OR sm.maps_to_concept_id IN (@conceptIdsToExtract)
          "
  
  concept <- DatabaseConnector::renderTranslateQuerySql(
    connection = sourceConnection,
    sql = sql,
    vocabularyDatabaseSchema = sourceVocabularyDatabaseSchema,
    conceptIdsToExtract = paste(conceptIdsToExtract, collapse = ","),
    resultsDatabaseSchema = sourceResultsDatabaseSchema,
    stratifiedCodeCountsTable = paste0("stratified_", codeCountsTable)
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

  # stratified persons bridge table — scoped to personBridgeConceptIds (which may be
  # narrower than conceptIdsToExtract) and capped to maxPersonsPerConcept distinct
  # persons per target concept, deterministically (lowest person_id first). A common
  # real-world code can otherwise have hundreds of thousands of person rows; kept
  # persons still bring all of their stratum rows for that concept, so per-person
  # detail is never truncated, only the number of persons covered.
  sql <- "
    WITH target_pairs AS (
        SELECT concept_id AS target_concept_id, person_id
        FROM @resultsDatabaseSchema.stratified_persons
        WHERE concept_id IN (@personConceptIdsToExtract)
        UNION
        SELECT maps_to_concept_id AS target_concept_id, person_id
        FROM @resultsDatabaseSchema.stratified_persons
        WHERE maps_to_concept_id IN (@personConceptIdsToExtract)
    ),
    ranked_pairs AS (
        SELECT target_concept_id, person_id,
               ROW_NUMBER() OVER (PARTITION BY target_concept_id ORDER BY person_id) AS rn
        FROM target_pairs
    ),
    kept_pairs AS (
        SELECT DISTINCT target_concept_id, person_id FROM ranked_pairs WHERE rn <= @maxPersonsPerConcept
    )
    SELECT DISTINCT sp.*
    FROM @resultsDatabaseSchema.stratified_persons sp
    INNER JOIN kept_pairs kp
      ON kp.person_id = sp.person_id
     AND (kp.target_concept_id = sp.concept_id OR kp.target_concept_id = sp.maps_to_concept_id)
  "
  stratifiedPersons <- DatabaseConnector::renderTranslateQuerySql(
    connection = sourceConnection,
    sql = sql,
    resultsDatabaseSchema = sourceResultsDatabaseSchema,
    personConceptIdsToExtract = paste(personConceptIdsToExtract, collapse = ","),
    maxPersonsPerConcept = maxPersonsPerConcept
  ) |>
    tibble::as_tibble()

  targetConnection |> DatabaseConnector::insertTable(
    tableName = "stratified_persons",
    data = stratifiedPersons,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
  )

  # stratified measurements table — event-level measured values for the extracted concepts,
  # capped to maxEventsPerConcept rows per (concept, unit). The cap is a systematic sample
  # over the value distribution (NTILE buckets of equal count, one row kept per bucket) so
  # the fixture's histogram keeps the shape of the real one; note there is deliberately NO
  # DISTINCT here, since two events sharing a value and stratum are two separate events and
  # collapsing them would corrupt the counts.
  sql <- "
    WITH target_measurements AS (
        SELECT
            sm.concept_id AS concept_id,
            sm.maps_to_concept_id AS maps_to_concept_id,
            sm.visit_group_concept_id AS visit_group_concept_id,
            sm.calendar_year AS calendar_year,
            sm.gender_concept_id AS gender_concept_id,
            sm.age_decile AS age_decile,
            sm.unit_concept_id AS unit_concept_id,
            sm.value_as_number AS value_as_number,
            CASE WHEN sm.concept_id IN (@conceptIdsToExtract) THEN sm.concept_id
                 ELSE sm.maps_to_concept_id END AS target_concept_id
        FROM @resultsDatabaseSchema.stratified_measurements sm
        WHERE sm.concept_id IN (@conceptIdsToExtract)
           OR sm.maps_to_concept_id IN (@conceptIdsToExtract)
    ),
    bucketed AS (
        SELECT tm.*,
               NTILE(@maxEventsPerConcept) OVER (
                   PARTITION BY tm.target_concept_id, tm.unit_concept_id
                   ORDER BY tm.value_as_number
               ) AS value_bucket
        FROM target_measurements tm
    ),
    ranked AS (
        SELECT b.*,
               ROW_NUMBER() OVER (
                   PARTITION BY b.target_concept_id, b.unit_concept_id, b.value_bucket
                   ORDER BY b.value_as_number
               ) AS rn
        FROM bucketed b
    )
    SELECT concept_id, maps_to_concept_id, visit_group_concept_id, calendar_year,
           gender_concept_id, age_decile, unit_concept_id, value_as_number
    FROM ranked
    WHERE rn = 1
  "
  stratifiedMeasurements <- DatabaseConnector::renderTranslateQuerySql(
    connection = sourceConnection,
    sql = sql,
    resultsDatabaseSchema = sourceResultsDatabaseSchema,
    conceptIdsToExtract = paste(conceptIdsToExtract, collapse = ","),
    maxEventsPerConcept = maxEventsPerConcept
  ) |>
    tibble::as_tibble()

  targetConnection |> DatabaseConnector::insertTable(
    tableName = "stratified_measurements",
    data = stratifiedMeasurements,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
  )

  # observed_persons_counts_stratified -- population-at-risk denominator, ~1k
  # rows (calendar_year x gender_concept_id x age_decile), no concept scoping
  # or person cap applies: extracted verbatim
  sql <- "SELECT DISTINCT opc.* FROM @resultsDatabaseSchema.observed_persons_counts_stratified opc"
  observedPersonsCounts <- DatabaseConnector::renderTranslateQuerySql(
    connection = sourceConnection,
    sql = sql,
    resultsDatabaseSchema = sourceResultsDatabaseSchema
  ) |>
    tibble::as_tibble()

  targetConnection |> DatabaseConnector::insertTable(
    tableName = "observed_persons_counts_stratified",
    data = observedPersonsCounts,
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
