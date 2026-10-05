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

test_that("getPersonCountsFilters gives exact pooled breakdown counts on a hand-built fixture (synthetic)", {
  # Checks the actual breakdown ARITHMETIC, not just shape, against
  # .buildSyntheticPersonCountsHandler() (tests/testthat/helper.R): 6 persons,
  # 2 of whom (1 and 6) carry events under both concept trees — the pooled
  # population for "100SD,200MD" is all 6 persons, with every stratum count
  # hand-countable from the fixture's rows.
  CDMdbHandler <- .buildSyntheticPersonCountsHandler()

  result <- getPersonCountsFilters(CDMdbHandler, conceptIds = "100SD,200MD")

  expected <- tibble::tribble(
    ~filter, ~stratum, ~person_counts,
    "sex", 1L, 3L,
    "sex", 2L, 3L,
    "age", 1L, 3L,
    "age", 2L, 2L,
    "age", 3L, 1L,
    "visit", 0L, 5L,
    "visit", 5L, 1L,
    "year", 2010L, 3L,
    "year", 2011L, 2L,
    "year", 2012L, 1L
  )

  result |>
    dplyr::mutate(stratum = as.integer(stratum), person_counts = as.integer(person_counts)) |>
    dplyr::select(filter, stratum, person_counts) |>
    dplyr::arrange(filter, stratum) |>
    expect_equal(expected |> dplyr::arrange(filter, stratum))
})

test_that("getPersonCountsFilters reciprocal filtering matches hand-computed subsets on a hand-built fixture (synthetic)", {
  # sexStratum=1 (male) restricts the OTHER dimensions to persons {1,3,5}: one
  # male in each of the three age deciles.
  CDMdbHandler <- .buildSyntheticPersonCountsHandler()

  result <- getPersonCountsFilters(CDMdbHandler, conceptIds = "100SD,200MD", sexStratum = 1L)

  ageCounts <- result |>
    dplyr::filter(filter == "age") |>
    dplyr::mutate(stratum = as.integer(stratum), person_counts = as.integer(person_counts)) |>
    dplyr::arrange(stratum) |>
    dplyr::select(stratum, person_counts)

  expected <- tibble::tribble(
    ~stratum, ~person_counts,
    1L, 1L,
    2L, 1L,
    3L, 1L
  )
  ageCounts |> expect_equal(expected)

  # sex's OWN breakdown is computed without its own filter applied, so it still
  # sums to all 6 persons regardless of sexStratum.
  result |>
    dplyr::filter(filter == "sex") |>
    dplyr::mutate(person_counts = as.integer(person_counts)) |>
    dplyr::pull(person_counts) |>
    sum() |>
    expect_equal(6L)
})

test_that("getPersonCountsFilters selected flags mark exactly the passed-in filter values on a hand-built fixture (synthetic)", {
  CDMdbHandler <- .buildSyntheticPersonCountsHandler()

  result <- getPersonCountsFilters(
    CDMdbHandler,
    conceptIds = "100SD,200MD",
    sexStratum = 1L,
    yearsRange = c(2010L, 2010L)
  )

  result |>
    dplyr::filter(filter == "sex") |>
    dplyr::arrange(as.integer(stratum)) |>
    dplyr::pull(selected) |>
    expect_equal(c(TRUE, FALSE)) # stratum 1 (male) selected, stratum 2 (female) not

  result |>
    dplyr::filter(filter == "year") |>
    dplyr::arrange(as.integer(stratum)) |>
    dplyr::pull(selected) |>
    expect_equal(c(TRUE, FALSE, FALSE)) # 2010 selected, 2011/2012 not

  # age/visit had no filter of their own passed in -> nothing selected
  result |>
    dplyr::filter(filter %in% c("age", "visit")) |>
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
