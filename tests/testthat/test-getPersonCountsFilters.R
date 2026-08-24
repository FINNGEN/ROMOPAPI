# NOTE: these tests read the `stratified_persons` bridge table. Against
# OnlyCounts-FinnGen they will fail until inst/testdata/data/FinnGenR13_countsOnly.sqlite
# is regenerated via inst/testdata/data/createTestingData.R (requires BigQuery
# access to AtlasDevelopment-full) — see development/STRUCTURE.md.

test_that("getPersonCountsFilters rejects malformed conceptIds tokens", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(getPersonCountsFilters(CDMdbHandler, conceptIds = ""))
  expect_error(getPersonCountsFilters(CDMdbHandler, conceptIds = "abc"))
  expect_error(getPersonCountsFilters(CDMdbHandler, conceptIds = "317009X"))
})

test_that("getPersonCountsFilters works", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsFilters_memoise)

  filterPersonCounts <- getPersonCountsFilters(CDMdbHandler, conceptIds = "317009SD")

  filterPersonCounts |>
    colnames() |>
    expect_equal(c("filter", "stratum", "person_counts", "selected"))

  filterPersonCounts |>
    dplyr::pull(filter) |>
    unique() |>
    (\(x) expect_true(all(x %in% c("sex", "age", "visit", "year"))))()

  filterPersonCounts |>
    dplyr::filter(is.na(filter) | is.na(stratum) | is.na(person_counts) | is.na(selected)) |>
    nrow() |>
    expect_equal(0)

  # no filters passed in -> nothing is marked selected
  filterPersonCounts |>
    dplyr::pull(selected) |>
    any() |>
    expect_false()
})

test_that("getPersonCountsFilters marks the selected stratum and cross-filters the other dimensions", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsFilters_memoise)

  full <- getPersonCountsFilters(CDMdbHandler, conceptIds = "317009SD")
  sexStrata <- full |> dplyr::filter(filter == "sex") |> dplyr::pull(stratum)

  filtered <- getPersonCountsFilters(CDMdbHandler, conceptIds = "317009SD", sexStratum = sexStrata[1])

  # the sex row(s) matching the passed-in filter are selected, all others are not
  filtered |>
    dplyr::filter(filter == "sex") |>
    dplyr::mutate(expected = stratum == sexStrata[1]) |>
    (\(x) expect_equal(x$selected, x$expected))()

  # the age/visit breakdown is now computed WITH the sex filter applied, so their
  # totals should be <= the unfiltered totals (never more restrictive filter -> more persons)
  ageTotalFull <- full |> dplyr::filter(filter == "age") |> dplyr::pull(person_counts) |> sum()
  ageTotalFiltered <- filtered |> dplyr::filter(filter == "age") |> dplyr::pull(person_counts) |> sum()
  ageTotalFiltered |> expect_lte(ageTotalFull)

  # but the sex row's own total (summed across ALL sex strata, own filter not applied
  # to itself) is unaffected by passing a sex filter
  sexTotalFull <- full |> dplyr::filter(filter == "sex") |> dplyr::pull(person_counts) |> sum()
  sexTotalFiltered <- filtered |> dplyr::filter(filter == "sex") |> dplyr::pull(person_counts) |> sum()
  sexTotalFiltered |> expect_equal(sexTotalFull)
})

test_that("getPersonCountsFilters marks selected year strata from yearsRange", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsFilters_memoise)

  full <- getPersonCountsFilters(CDMdbHandler, conceptIds = "317009SD")
  years <- full |> dplyr::filter(filter == "year") |> dplyr::pull(stratum)
  skip_if(length(years) == 0)

  narrow <- getPersonCountsFilters(CDMdbHandler, conceptIds = "317009SD", yearsRange = c(min(years), min(years)))

  narrow |>
    dplyr::filter(filter == "year") |>
    dplyr::mutate(expected = stratum == min(years)) |>
    (\(x) expect_equal(x$selected, x$expected))()

  # sex/age/visit breakdowns are now restricted to that single year
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
    getPersonCountsFilters(CDMdbHandler, conceptIds = "317009SD", yearsRange = c(2020L, 2015L))
  )
})

test_that("getPersonCountsFilters returns error if concept id has no descendants", {
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
      conceptIds = "1000000000SD"
    )
  )
})
