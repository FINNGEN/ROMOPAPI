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

  # check that descendant_event_counts is the same as event_counts for all concept_id_2
  countsDescendants <- all |>
    dplyr::filter(relationship_id != "Parent" & relationship_id != "Mapped from") |>
    dplyr::group_by(concept_id_1, concept_id_2, relationship_id) |>
    dplyr::summarise(
      event_counts = sum(event_counts),
      descendant_event_counts = sum(descendant_event_counts),
      .groups = "drop"
    ) |>
    dplyr::arrange(relationship_id)

  sumDescendants <- countsDescendants |>
    dplyr::pull(event_counts) |>
    sum()

  rootDescendants <- countsDescendants |>
    dplyr::filter(relationship_id == "Root") |>
    dplyr::pull(descendant_event_counts)

  expect_equal(rootDescendants, sumDescendants)

  # check that all the mapped from event_counts are the same as the descendant_event_counts
  sumEventsMappedFrom <- all |>
    dplyr::filter(relationship_id == "Mapped from") |>
    dplyr::group_by(concept_id_1, concept_id_2) |>
    dplyr::summarise(
      event_counts = sum(event_counts),
      .groups = "drop"
    )
  famillyEvents <- all |>
    dplyr::group_by(concept_id_1, concept_id_2) |>
    dplyr::summarise(
      event_counts = sum(event_counts),
      .groups = "drop"
    )

  dplyr::inner_join(
    sumEventsMappedFrom,
    famillyEvents,
    by = c("concept_id_1" = "concept_id_2")
  ) |>
    dplyr::group_by(concept_id_1, event_counts.y) |>
    dplyr::summarise(
      event_counts.x = sum(event_counts.x), .groups = "drop"
    ) |>
    dplyr::filter(event_counts.x != event_counts.y) |>
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
