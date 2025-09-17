# create test for getCDMSource

test_that("getCDMSource works", {
  # only works in a full CDM database
  skip_if(testingDatabase == "OnlyCounts-FinnGen")

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  cdm_source <- getCDMSource(CDMdbHandler)

  # Check column names 
  cdm_source |>
    nrow() |>
    expect_equal(1)

})
