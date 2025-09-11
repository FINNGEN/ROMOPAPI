test_that("getCodeCounts works", {
  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler$finalize()
  })

  suppressWarnings(
    result <- getCodeCounts(
      CDMdbHandler,
      conceptIds = c(317009)
    )
  )

  result$code_counts |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)
  result$concept_relationships |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)
  result$concepts |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

  all <- result$concept_relationships |>
    dplyr::left_join(result$code_counts, by = c("concept_id_2" = "concept_id")) |>
    dplyr::left_join(result$concepts, by = c("concept_id_2" = "concept_id"))

  if (testingDatabase == "Eunomia-FinnGen-counts") {
    result$concept_relationships |>
      nrow() |>
      expect_equal(23)
  }
  if (testingDatabase == "AtlasDevelopment-DBI") {
    result$concept_relationships |>
      nrow() |>
      expect_equal(35)
  }

  concept_relationships_ids <- c(
    result$concept_relationships |>
      dplyr::pull(concept_id_2),
    result$concept_relationships |>
      dplyr::pull(concept_id_1)
  ) |>
    unique() |>
    sort()

  # check that all concept_id_2 are in the concepts
  result$concepts |>
    dplyr::distinct(concept_id) |>
    dplyr::pull(concept_id) |>
    sort() |>
    expect_equal(concept_relationships_ids)

  # check that all concept_id_2 are in the code_counts
  result$code_counts |>
    dplyr::distinct(concept_id) |>
    dplyr::pull(concept_id) |>
    sort() |>
    expect_equal(concept_relationships_ids)

  # check that descendant_record_counts is the same as record_counts for all concept_id_2
  countsDescendants <- all |>
    dplyr::filter(relationship_id != "Parent" & relationship_id != "Mapped from") |>
    dplyr::group_by(concept_id_1, concept_id_2, relationship_id) |>
    dplyr::summarise(
      record_counts = sum(record_counts),
      descendant_record_counts = sum(descendant_record_counts),
      .groups = "drop"
    ) |>
    dplyr::arrange(relationship_id)

  sumDescendants <- countsDescendants |>
    dplyr::pull(record_counts) |>
    sum()

  rootDescendants <- countsDescendants |>
    dplyr::filter(relationship_id == "Root") |>
    dplyr::pull(descendant_record_counts)

  expect_equal(rootDescendants, sumDescendants)

  # check that all the mapped from record_counts are the same as the descendant_record_counts
  sumEventsMappedFrom <- all |>
    dplyr::filter(relationship_id == "Mapped from") |>
    dplyr::group_by(concept_id_1, concept_id_2) |>
    dplyr::summarise(
      record_counts = sum(record_counts),
      .groups = "drop"
    )
  famillyEvents <- all |>
    dplyr::group_by(concept_id_1, concept_id_2) |>
    dplyr::summarise(
      record_counts = sum(record_counts),
      .groups = "drop"
    )

  dplyr::inner_join(
    sumEventsMappedFrom,
    famillyEvents,
    by = c("concept_id_1" = "concept_id_2")
  ) |>
    dplyr::group_by(concept_id_1, record_counts.y) |>
    dplyr::summarise(
      record_counts.x = sum(record_counts.x), .groups = "drop"
    ) |>
    dplyr::filter(record_counts.x != record_counts.y) |>
    nrow() |>
    expect_equal(0)
})


test_that("getListOfConcepts works", {
  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler$finalize()
  })

  concepts <- getListOfConcepts(CDMdbHandler)

  expect_true(all(c("concept_id", "concept_name", "vocabulary_id", "standard_concept") %in% colnames(concepts)))
  expect_gt(nrow(concepts), 0)
  # Validate column types
  expect_type(concepts$concept_id, "double")
  expect_type(concepts$concept_name, "character")
  expect_type(concepts$vocabulary_id, "character")
  expect_type(concepts$standard_concept, "logical")
})



test_that("getCodeCounts works for TEMP ATC codes", {
  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler$finalize()
  })

  suppressWarnings(
    result <- getCodeCounts(
      CDMdbHandler,
      conceptIds = c(21600744 + 2100000000)
    )
  )

  result$concept_relationships |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(10)

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
    317009L,               "0-0",   1L,
    257581L,               "1-1",   1L,
    4110051L,              "1-1",   1L,
    4145497L,              "1-1",   1L,
    4191479L,              "1-1",   1L,
    312950L,               "2-2",   1L,
    42538744L,             "2-2",   2L,
    45769438L,             "2-2",   1L,
    45773005L,             "2-2",   2L,
    37116845L,             "3-3",   1L,
    45769441L,             "3-3",   2L
  )

  expect_equal(ancestorTable, result)

  ancestorTable <- .familyTreeToAncestorTable(familyTree, 312950)
  result <- tibble::tribble(
    ~descendant_concept_id, ~levels, ~paths,
    312950L,               "0-0",   1L,
    45769441L,             "1-1",   1L
  )
  expect_equal(ancestorTable, result)


  ancestorTable <- .familyTreeToAncestorTable(familyTree, 45769441)
  result <- tibble::tribble(
    ~descendant_concept_id, ~levels, ~paths,
    45769441L,             "0-0",   1L
  )

  expect_equal(ancestorTable, result)
})
