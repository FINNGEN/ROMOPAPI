# NOTE: these tests read the `stratified_persons` bridge table. Against
# OnlyCounts-FinnGen they will fail until inst/testdata/data/FinnGenR13_countsOnly.sqlite
# is regenerated via inst/testdata/data/createTestingData.R (requires BigQuery
# access to AtlasDevelopment-full) — see development/STRUCTURE.md.

test_that("getPersonCountsFilters works", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getConceptTree_memoise)
  memoise::forget(getPersonCountsFilters_memoise)

  filterPersonCounts <- getPersonCountsFilters(CDMdbHandler, conceptId = 317009L)

  filterPersonCounts |>
    colnames() |>
    expect_equal(c("filter", "stratum", "person_counts"))

  filterPersonCounts |>
    dplyr::pull(filter) |>
    unique() |>
    (\(x) expect_true(all(x %in% c("sex", "age", "visit"))))()

  filterPersonCounts |>
    dplyr::filter(is.na(filter) | is.na(stratum) | is.na(person_counts)) |>
    nrow() |>
    expect_equal(0)
})

test_that("getPersonCountsFilters narrows totals to the given yearsRange", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getConceptTree_memoise)
  memoise::forget(getPersonCountsFilters_memoise)

  full <- getPersonCountsFilters(CDMdbHandler, conceptId = 317009L)
  narrow <- getPersonCountsFilters(CDMdbHandler, conceptId = 317009L, yearsRange = c(2015L, 2020L))

  fullTotal <- full |> dplyr::filter(filter == "sex") |> dplyr::pull(person_counts) |> sum()
  narrowTotal <- narrow |> dplyr::filter(filter == "sex") |> dplyr::pull(person_counts) |> sum()

  narrowTotal |> expect_lte(fullTotal)
})

test_that("getPersonCountsFilters rejects an inverted yearsRange", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCountsFilters(CDMdbHandler, conceptId = 317009L, yearsRange = c(2020L, 2015L))
  )
})

test_that("getPersonCountsFilters returns error if conceptId is not found", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCountsFilters(
      CDMdbHandler,
      conceptId = c(1000000000)
    )
  )
})
