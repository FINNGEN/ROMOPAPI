test_that("getCodeCounts works", {

  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({CDMdbHandler$finalize()})

  createCodeCountsTable(CDMdbHandler)
  
  result <- getCodeCounts(
    CDMdbHandler, 
    conceptIds = c(317009)
  )

  result$code_counts |> dplyr::count() |> dplyr::pull(n) |> expect_gt(0)
  result$concepts |> dplyr::count() |> dplyr::pull(n) |> expect_gt(0)
  result$concept_relationships |> dplyr::count() |> dplyr::pull(n) |> expect_gt(0)

})
