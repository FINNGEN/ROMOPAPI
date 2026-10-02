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

test_that("getConceptTree works", {
  # post-counts test: reads the pre-built stratified_code_counts table
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getConceptTree_memoise)

  result <- getConceptTree(CDMdbHandler, conceptId = 317009L, codeCountsTable = "code_counts")

  result |>
    names() |>
    expect_equal(c("family_tree", "concept_ids"))

  result$family_tree |>
    colnames() |>
    expect_equal(c("parent_concept_id", "child_concept_id", "levels", "paths"))

  result$family_tree |>
    nrow() |>
    expect_gt(0)

  # concept_ids excludes the reverse-parent "-1" edge, includes the root itself
  result$concept_ids |>
    (\(x) expect_true(317009 %in% x))()

  result$family_tree |>
    dplyr::filter(levels == "-1") |>
    dplyr::pull(child_concept_id) |>
    (\(x) expect_false(any(x %in% result$concept_ids)))()

  # memoised twin returns the same result and shares the cache with getConceptTree
  resultMemoised <- getConceptTree_memoise(CDMdbHandler, conceptId = 317009L, codeCountsTable = "code_counts")
  expect_equal(result, resultMemoised)
})
