test_that("getConceptRelationships works", {
  # post-counts test: reads the pre-built code_counts table
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  # OnlyCounts-FinnGen ships it; AtlasDevelopment-5k has it built by setup.R.
  codeCountsTable <- "code_counts"

  # memoise cache is keyed without CDMdbHandler — drop entries that may exist from earlier runs
  memoise::forget(getConceptTree_memoise)
  memoise::forget(getConceptRelationships_memoise)

  suppressWarnings(
    result <- getConceptRelationships(
      CDMdbHandler,
      conceptId = c(317009),
      codeCountsTable = codeCountsTable
    )
  )

  concept_relationships <- result$concept_relationships
  concepts <- result$concepts

  concept_relationships |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)
  concepts |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

  #
  # concept_relationships
  #

  # Check column names
  concept_relationships |>
    colnames() |>
    expect_equal(c("parent_concept_id", "child_concept_id", "levels", "concept_class_id"))

  # columns not empty
  concept_relationships |>
    dplyr::filter(is.na(parent_concept_id) | is.na(child_concept_id) | is.na(levels) ) |>
    nrow() |>
    expect_equal(0)

  # check that all the child_concept_id are in parent_concept_id
  concept_relationships |>
    dplyr::anti_join(concept_relationships, by = c("parent_concept_id" = "child_concept_id")) |>
    nrow() |>
    expect_equal(0)

  # check that all concept_id in concept_relationships are in the concepts
  concept_relationships |>
    dplyr::anti_join(concepts, by = c("child_concept_id" = "concept_id")) |>
    nrow() |>
    expect_equal(0)

  #
  # concepts
  #

  # Check column names
  concepts |>
    colnames() |>
    expect_equal(c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", "record_counts", "descendant_record_counts", "person_counts", "descendant_person_counts"))

  # columns not empty
  concepts |>
    dplyr::filter(is.na(concept_id) | is.na(concept_name) | is.na(domain_id) | is.na(vocabulary_id) | is.na(standard_concept) | is.na(concept_code) | is.na(record_counts) | is.na(descendant_record_counts)) |>
      nrow() |>
      expect_equal(0)
})

test_that("getConceptRelationships returns error if conceptId is not found", {
  # post-counts test: reads the pre-built code_counts table
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getConceptRelationships(
      CDMdbHandler,
      conceptId = c(1000000000)
    )
  )
})

test_that("getConceptRelationships and getCodeCountsStratified never link to concept 0 (synthetic)", {
  # Self-contained regression test on a throw-away SQLite results schema. Events with a
  # standard concept but no source concept (maps_to_concept_id = 0), or with a source
  # concept but no standard one (concept_id = 0), must not create a 'Mapped from' /
  # 'Maps to' edge to concept 0 nor a concept 0 row in the stratified counts.
  # 7000xx are fake ids that collide with nothing in a real vocabulary.
  dbPath <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(dbPath))
  config <- list(
    database = list(databaseId = "SYN", databaseName = "Synthetic", databaseDescription = "Synthetic"),
    connection = list(connectionDetailsSettings = list(dbms = "sqlite", server = dbPath)),
    cdm = list(cdmDatabaseSchema = "main", vocabularyDatabaseSchema = "main", resultsDatabaseSchema = "main")
  )
  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(config, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })
  connection <- CDMdbHandler$connectionHandler$getConnection()
  insert <- function(tableName, data) {
    DatabaseConnector::insertTable(
      connection,
      tableName = tableName, data = data,
      dropTableIfExists = TRUE, createTable = TRUE, tempTable = FALSE
    )
  }

  # 700000 is the parent of 700001 (standard measurement), 700002 (its source code)
  # and 700003 (a source code whose standard concept is unmapped)
  insert("concept_ancestor", tibble::tibble(
    ancestor_concept_id = c(700000L, 700001L, 700002L, 700003L, 700000L, 700000L, 700000L),
    descendant_concept_id = c(700000L, 700001L, 700002L, 700003L, 700001L, 700002L, 700003L),
    min_levels_of_separation = c(0L, 0L, 0L, 0L, 1L, 1L, 1L),
    max_levels_of_separation = c(0L, 0L, 0L, 0L, 1L, 1L, 1L)
  ))
  insert("concept", tibble::tibble(
    concept_id = 700000:700003,
    concept_name = paste("concept", 700000:700003),
    domain_id = "Measurement",
    vocabulary_id = "SYN",
    concept_class_id = "Lab",
    standard_concept = c("S", "S", NA, NA),
    concept_code = paste0("c", 700000:700003)
  ))
  insert("stratified_code_counts", tibble::tibble(
    concept_id = c(700001L, 700001L, 0L),
    maps_to_concept_id = c(700002L, 0L, 700003L), # source known / no source / unmapped standard
    visit_group_concept_id = 0L,
    calendar_year = 2010L,
    gender_concept_id = 8507L,
    age_decile = 3L,
    record_counts = c(5L, 7L, 4L)
  ))
  # as rebuilt by createCodeCountsTable.sql: no concept 0 row
  insert("code_counts", tibble::tibble(
    concept_id = 700001:700003,
    record_counts = c(12L, 5L, 4L),
    descendant_record_counts = c(12L, 5L, 4L),
    number_of_descendants = 1L,
    person_counts = 1L,
    descendant_person_counts = 1L
  ))

  # memoise cache is keyed without CDMdbHandler
  memoise::forget(getConceptTree_memoise)

  for (conceptId in c(700001, 700003)) {
    relationships <- getConceptRelationships(CDMdbHandler, conceptId = conceptId)
    stratified <- getCodeCountsStratified(CDMdbHandler, conceptId = conceptId)

    expect_false(0 %in% relationships$concept_relationships$parent_concept_id)
    expect_false(0 %in% relationships$concept_relationships$child_concept_id)
    expect_false(0 %in% relationships$concepts$concept_id)
    expect_false(0 %in% stratified$concept_id)
  }

  # the real mappings are still there
  suppressWarnings(
    relationships <- getConceptRelationships(CDMdbHandler, conceptId = 700001)
  )
  relationships$concept_relationships |>
    dplyr::filter(levels == "Mapped from", parent_concept_id == 700001, child_concept_id == 700002) |>
    nrow() |>
    expect_equal(1)
})
