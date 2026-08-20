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
