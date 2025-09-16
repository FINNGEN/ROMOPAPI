test_that("getCodeCounts works", {
  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  suppressWarnings(
    result <- getCodeCounts(
      CDMdbHandler,
      conceptId = c(317009)
    )
  )

  concept_relationships <- result$concept_relationships
  stratified_code_counts <- result$stratified_code_counts
  concepts <- result$concepts

  stratified_code_counts |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)
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
    dplyr::filter(is.na(parent_concept_id) | is.na(child_concept_id) | is.na(levels) | is.na(concept_class_id)) |>
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
  # stratified_code_counts
  #

  # Check column names 
  stratified_code_counts |>
    colnames() |>
    expect_equal(c("concept_id", "calendar_year", "gender_concept_id", "age_decile", "node_record_counts", "node_descendant_record_counts"))

  # columns not empty
  stratified_code_counts |>
    dplyr::filter(is.na(concept_id) | is.na(calendar_year) | is.na(gender_concept_id) | is.na(age_decile) | is.na(node_record_counts) | is.na(node_descendant_record_counts)) |>
    nrow() |>
    expect_equal(0)

  # check that all the concept_id are in the concepts
  stratified_code_counts |>
    dplyr::anti_join(concepts, by = c("concept_id" = "concept_id")) |>
    nrow() |>
    expect_equal(0)

  #
  # concepts
  #

  # Check column names 
  concepts |>
    colnames() |>
    expect_equal(c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", "record_counts", "descendant_record_counts"))

  # columns not empty
  concepts |>
    dplyr::filter(is.na(concept_id) | is.na(concept_name) | is.na(domain_id) | is.na(vocabulary_id) | is.na(concept_class_id) | is.na(standard_concept) | is.na(concept_code) | is.na(record_counts) | is.na(descendant_record_counts)) |>
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

test_that("getCodeCounts returns error if conceptId is not found", {
  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getCodeCounts(
      CDMdbHandler,
      conceptId = c(1000000000)
    )
  )

})




test_that(".familyTreeToAncestorTable works", {
  familyTree <- tibble::tribble(
    ~parent_concept_id, ~child_concept_id,
    45769438,           37116845,
    257581,             45769438,
    4145497,            45773005,
    42538744,           45769441,
    257581,             42538744,
    4191479,            42538744,
    312950,             45769441,
    317009,             4110051,
    317009,             257581,
    317009,             4191479,
    4191479,            312950,
    317009,             4145497,
    257581,             45773005,
    320136,             317009
  )

  ancestorTable <- .familyTreeToAncestorTable(familyTree, 317009)

  result <- tibble::tribble(
    ~descendant_concept_id, ~levels, ~paths,
    317009L, "0-0", 1L,
    257581L, "1-1", 1L,
    4110051L, "1-1", 1L,
    4145497L, "1-1", 1L,
    4191479L, "1-1", 1L,
    312950L, "2-2", 1L,
    42538744L, "2-2", 2L,
    45769438L, "2-2", 1L,
    45773005L, "2-2", 2L,
    37116845L, "3-3", 1L,
    45769441L, "3-3", 2L
  )

  expect_equal(ancestorTable, result)

  ancestorTable <- .familyTreeToAncestorTable(familyTree, 312950)
  result <- tibble::tribble(
    ~descendant_concept_id, ~levels, ~paths,
    312950L, "0-0", 1L,
    45769441L, "1-1", 1L
  )
  expect_equal(ancestorTable, result)


  ancestorTable <- .familyTreeToAncestorTable(familyTree, 45769441)
  result <- tibble::tribble(
    ~descendant_concept_id, ~levels, ~paths,
    45769441L, "0-0", 1L
  )

  expect_equal(ancestorTable, result)
})
