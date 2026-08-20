test_that("getAllConceptsInfo works", {
  # post-counts test: reads the pre-built code_counts table
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  suppressWarnings(
    result <- getAllConceptsInfo(CDMdbHandler)
  )

  # Check that we have at least some concepts
  result |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

  # Check column names match expected structure
  expected_columns <- c("concept_id", "concept_name", "domain_id", "vocabulary_id",
                       "concept_class_id", "standard_concept", "concept_code")

  result |>
    colnames() |>
    expect_equal(expected_columns)

  # Check that no required columns are empty/NA
  result |>
    dplyr::filter(is.na(concept_id) | is.na(concept_name) | is.na(domain_id) |
                 is.na(vocabulary_id) | is.na(concept_class_id) |
                 is.na(concept_code)) |>
    nrow() |>
    expect_equal(0)

  # Check that standard_concept is logical (TRUE/FALSE)
  result |>
    dplyr::pull(standard_concept) |>
    expect_type("logical")

  # Check that concept_id is numeric
  result |>
    dplyr::pull(concept_id) |>
    expect_type("double")

  # Check that all concept_ids are unique
  result |>
    dplyr::count(concept_id, sort = TRUE) |>
    dplyr::filter(n > 1) |>
    nrow() |>
    expect_equal(0)
})
