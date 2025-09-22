test_that("getConceptsWithCodeCounts works", {
  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  suppressWarnings(
    result <- getConceptsWithCodeCounts(CDMdbHandler)
  )
  
  # Check that we have at least some concepts
  result |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

  # Check column names match expected structure
  expected_columns <- c("concept_id", "concept_name", "domain_id", "vocabulary_id", 
                       "concept_class_id", "standard_concept", "concept_code", 
                       "record_counts", "descendant_record_counts")
  
  result |>
    colnames() |>
    expect_equal(expected_columns)

  # Check that no required columns are empty/NA
  result |>
    dplyr::filter(is.na(concept_id) | is.na(concept_name) | is.na(domain_id) | 
                 is.na(vocabulary_id) | is.na(concept_class_id) | 
                 is.na(concept_code) | is.na(record_counts) | 
                 is.na(descendant_record_counts)) |>
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

  # Check that record_counts and descendant_record_counts are numeric and non-negative
  result |>
    dplyr::filter(record_counts < 0 | descendant_record_counts < 0) |>
    nrow() |>
    expect_equal(0)

  # Check that all concept_ids are unique
  result |>
    dplyr::count(concept_id, sort = TRUE) |>
    dplyr::filter(n > 1) |>
    nrow() |>
    expect_equal(0)
})
