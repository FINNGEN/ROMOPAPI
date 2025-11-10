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
  api_info |> names() |>
    expect_equal(c("cdm_source_name", "cdm_source_abbreviation", "vocabulary_version", "romop_api_version"))
   api_info$romop_api_version |>
    expect_equal(as.character(utils::packageVersion("ROMOPAPI")))

})
