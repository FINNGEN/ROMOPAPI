test_that("getVisitTypeNames works", {
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
    expect_equal(c("visitGroupConceptId", "conceptCode", "conceptName"))

  # Check that columns are not empty
  visitTypeNames |>
    dplyr::filter(is.na(visitGroupConceptId) | is.na(conceptCode) | is.na(conceptName)) |>
    nrow() |>
    expect_equal(0)

  # Check that visitGroupConceptId is not 0
  visitTypeNames |>
    dplyr::filter(visitGroupConceptId == 0) |>
    nrow() |>
    expect_equal(0)

  # Check that visitGroupConceptId values are unique
  visitTypeNames |>
    dplyr::distinct(visitGroupConceptId) |>
    nrow() |>
    expect_equal(nrow(visitTypeNames))
})

test_that("getVisitTypeNames_memoise works", {
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

