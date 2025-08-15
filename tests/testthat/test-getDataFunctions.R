test_that("getCodeCounts works", {
  skip_if(testingDatabase != "Eunomia-FinnGen")

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

test_that("getListOfConcepts works", {
  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({CDMdbHandler$finalize()})

  createCodeCountsTable(CDMdbHandler)

  concepts <- getListOfConcepts(CDMdbHandler)

  expect_true(all(c("concept_id", "concept_name", "vocabulary_id", "standard_concept") %in% colnames(concepts)))
  expect_gt(nrow(concepts), 0)
  # Validate column types
  expect_type(concepts$concept_id, "double")
  expect_type(concepts$concept_name, "character") 
  expect_type(concepts$vocabulary_id, "character")
  expect_type(concepts$standard_concept, "logical")
})

test_that("getCodeCounts works for ATC codes", {
  skip_if(testingDatabase != "Eunomia-FinnGen")

  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({CDMdbHandler$finalize()})

  createCodeCountsTable(CDMdbHandler)
  
  result <- getCodeCounts(
    CDMdbHandler, 
    conceptIds = c(21604344)
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

test_that("getListOfConcepts works", {
  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({CDMdbHandler$finalize()})

  createCodeCountsTable(CDMdbHandler)

  concepts <- getListOfConcepts(CDMdbHandler)

  expect_true(all(c("concept_id", "concept_name", "vocabulary_id", "standard_concept") %in% colnames(concepts)))
  expect_gt(nrow(concepts), 0)
  # Validate column types
  expect_type(concepts$concept_id, "double")
  expect_type(concepts$concept_name, "character") 
  expect_type(concepts$vocabulary_id, "character")
  expect_type(concepts$standard_concept, "logical")
})


c <- CDMdbHandler$connectionHandler$getConnection() |> dplyr::tbl("concept") 
c |> dplyr::count(vocabulary_id) |> print(n = Inf)

r  <- CDMdbHandler$connectionHandler$getConnection() |> dplyr::tbl("concept_relationship") 
r  |> dplyr::filter(concept_id_1 == 21604457) 






