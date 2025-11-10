# create test for getCDMSource

test_that("getAPIInfo works", {
  # only works in a full CDM databas

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  api_info <- getAPIInfo(CDMdbHandler)

  # Check column names 
  api_info$cdm_source_name |>
    expect_equal("FinnGen data freeze 13")
  api_info$cdm_source_abbreviation |>
    expect_equal("FinnGen-DF13")
  api_info$vocabulary_version |>
    expect_equal("v5.0 27-FEB-25")
  api_info$romop_api_version |>
    expect_equal(as.character(utils::packageVersion("ROMOPAPI")))

})
