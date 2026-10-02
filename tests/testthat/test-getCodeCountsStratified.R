test_that("getCodeCountsStratified works", {
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
  memoise::forget(getCodeCountsStratified_memoise)

  suppressWarnings(
    stratified_code_counts <- getCodeCountsStratified(
      CDMdbHandler,
      conceptId = c(317009),
      codeCountsTable = codeCountsTable
    )
  )

  stratified_code_counts |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

  # Check column names.
  expectedStratifiedCols <- c(
    "concept_id", "visit_group_concept_id", "calendar_year",
    "gender_concept_id", "age_decile",
    "node_record_counts", "node_descendant_record_counts"
  )
  stratified_code_counts |>
    colnames() |>
    expect_setequal(expectedStratifiedCols)

  # columns not empty
  stratified_code_counts |>
    dplyr::filter(is.na(concept_id) | is.na(visit_group_concept_id) | is.na(calendar_year) | is.na(gender_concept_id) | is.na(age_decile) | is.na(node_record_counts) | is.na(node_descendant_record_counts)) |>
    nrow() |>
    expect_equal(0)

  # check that all the concept_id are in the tree's concept relationships
  concepts <- getConceptRelationships(CDMdbHandler, conceptId = c(317009), codeCountsTable = codeCountsTable)$concepts
  stratified_code_counts |>
    dplyr::anti_join(concepts, by = c("concept_id" = "concept_id")) |>
    nrow() |>
    expect_equal(0)

  # check that the record_counts and descendant_record_counts are the same as the aggregated_counts
  aggregated_counts <- stratified_code_counts |>
    dplyr::group_by(concept_id) |>
    dplyr::summarise(node_record_counts = sum(node_record_counts), node_descendant_record_counts = sum(node_descendant_record_counts), .groups = "drop")
  concepts |>
    dplyr::left_join(aggregated_counts, by = "concept_id") |>
    nrow() |>
    expect_equal(nrow(concepts))
})

test_that("getCodeCountsStratified returns error if conceptId is not found", {
  # post-counts test: reads the pre-built code_counts table
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getCodeCountsStratified(
      CDMdbHandler,
      conceptId = c(1000000000)
    )
  )
})
