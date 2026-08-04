test_that("getVisitTypeNames works", {
  # post-counts test: needs the FinnGen visit groups in the counts tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(
    test_cohortTableHandlerConfig,
    loadConnectionChecksLevel = "basicChecks"
  )
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  suppressWarnings(
    visitTypeNames <- getVisitTypeNames(CDMdbHandler)
  )

  # Check that the function returns a tibble
  expect_s3_class(visitTypeNames, "tbl_df")

  # Check column names
  visitTypeNames |>
    colnames() |>
    expect_equal(c("visit_group_concept_id", "concept_code", "concept_name"))

  # Check that columns are not empty
  visitTypeNames |>
    dplyr::filter(is.na(visit_group_concept_id) | is.na(concept_code) | is.na(concept_name)) |>
    nrow() |>
    expect_equal(0)

  # Check that visit_group_concept_id is not 0
  visitTypeNames |>
    dplyr::filter(visit_group_concept_id == 0) |>
    nrow() |>
    expect_equal(0)

  # Check that visit_group_concept_id values are unique
  visitTypeNames |>
    dplyr::distinct(visit_group_concept_id) |>
    nrow() |>
    expect_equal(nrow(visitTypeNames))
})

test_that("getVisitTypeNames_memoise works", {
  # post-counts test: needs the FinnGen visit groups in the counts tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(
    test_cohortTableHandlerConfig,
    loadConnectionChecksLevel = "basicChecks"
  )
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  suppressWarnings(
    visitTypeNames1 <- getVisitTypeNames_memoise(CDMdbHandler)
  )

  suppressWarnings(
    visitTypeNames2 <- getVisitTypeNames_memoise(CDMdbHandler)
  )

  # Check that both calls return the same result
  expect_equal(visitTypeNames1, visitTypeNames2)
})

