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

  result$concept_relationships  |> nrow() |> expect_equal(8)

  concept_relationships_ids  <-  c(
        result$concept_relationships |>
            dplyr::pull(concept_id_2),
        result$concept_relationships |>
            dplyr::pull(concept_id_1)
    ) |>
        unique() |> 
        sort()
  
  result$concepts |> 
    dplyr::pull(concept_id) |> 
    sort() |> 
    expect_equal(concept_relationships_ids)

  result$code_counts |> dplyr::distinct(concept_id) |> 
    dplyr::pull(concept_id) |> 
    sort() |> 
    expect_equal(concept_relationships_ids)

})
