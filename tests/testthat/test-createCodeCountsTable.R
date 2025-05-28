test_that("createCodeCountsTable works", {

  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({CDMdbHandler$finalize()})

  createCodeCountsTable(CDMdbHandler)

  # - Check if the table was created
  code_counts <- CDMdbHandler$connectionHandler$tbl("code_counts")  

  code_counts |> dplyr::count() |> dplyr::pull(n) |> expect_gt(0)
  code_counts |> colnames() |> expect_equal(c("domain", "concept_id", "calendar_year", "gender_concept_id", "age_decile", "event_counts",  "person_counts", "total_person_counts"))
  code_counts |> dplyr::filter(person_counts > event_counts) |> dplyr::count() |> dplyr::pull(n) |> expect_equal(0)

})
