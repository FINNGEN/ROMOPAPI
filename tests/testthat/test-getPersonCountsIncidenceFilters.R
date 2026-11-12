# NOTE: these tests read the `stratified_persons` bridge table, already shipped
# in inst/testdata/data/FinnGenR13_countsOnly.sqlite — no fixture regeneration
# needed. Exact arithmetic is covered by
# .buildSyntheticIncidencePersonCountsHandler() (tests/testthat/helper.R).

test_that("getPersonCountsIncidenceFilters rejects malformed conceptIds tokens", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(getPersonCountsIncidenceFilters(CDMdbHandler, conceptIds = ""))
  expect_error(getPersonCountsIncidenceFilters(CDMdbHandler, conceptIds = "abc"))
  expect_error(getPersonCountsIncidenceFilters(CDMdbHandler, conceptIds = "317009X"))
})

test_that("getPersonCountsIncidenceFilters works", {
  # post-counts test: reads the pre-built stratified_persons table
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsIncidenceFilters_memoise)

  filterPersonCounts <- getPersonCountsIncidenceFilters(CDMdbHandler, conceptIds = "317009SD")

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

  filterPersonCounts |>
    dplyr::pull(selected) |>
    any() |>
    expect_false()
})

test_that("getPersonCountsIncidenceFilters's year total never exceeds getPersonCountsFilters'", {
  # invariant: incidence pools each person at most once per token (their first
  # year), raw Filters can count the same person again in a later year, so the
  # year breakdown's total is <= the raw Filters' year breakdown total.
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsIncidenceFilters_memoise)
  memoise::forget(getPersonCountsFilters_memoise)

  incidenceYearTotal <- getPersonCountsIncidenceFilters(CDMdbHandler, conceptIds = "317009SD") |>
    dplyr::filter(filter == "year") |>
    dplyr::pull(person_counts) |>
    sum()
  filtersYearTotal <- getPersonCountsFilters(CDMdbHandler, conceptIds = "317009SD") |>
    dplyr::filter(filter == "year") |>
    dplyr::pull(person_counts) |>
    sum()

  incidenceYearTotal |> expect_lte(filtersYearTotal)
})

test_that("getPersonCountsIncidenceFilters gives exact pooled breakdown counts on a hand-built fixture (synthetic)", {
  # Against .buildSyntheticIncidencePersonCountsHandler() (tests/testthat/helper.R):
  # "100SD"'s incident rows are person 1 (2010, gender1/age1/visit0), person 2
  # (2011, gender2/age2/visit7), person 3 (2011, gender1/age2/visit0) -- person
  # 1's SECOND (2012) record is excluded, so it contributes nothing here.
  CDMdbHandler <- .buildSyntheticIncidencePersonCountsHandler()

  result <- getPersonCountsIncidenceFilters(CDMdbHandler, conceptIds = "100SD")

  expected <- tibble::tribble(
    ~filter, ~stratum, ~person_counts,
    "sex", 1L, 2L,
    "sex", 2L, 1L,
    "age", 1L, 1L,
    "age", 2L, 2L,
    "visit", 0L, 2L,
    "visit", 7L, 1L,
    "year", 2010L, 1L,
    "year", 2011L, 2L
  )

  result |>
    dplyr::mutate(stratum = as.integer(stratum), person_counts = as.integer(person_counts)) |>
    dplyr::select(filter, stratum, person_counts) |>
    dplyr::arrange(filter, stratum) |>
    expect_equal(expected |> dplyr::arrange(filter, stratum))
})

test_that("getPersonCountsIncidenceFilters reciprocal filtering matches hand-computed subsets (synthetic)", {
  # restricting to gender 1 (persons 1 and 3) narrows the OTHER dimensions to
  # just their rows, while sex's OWN total (not filtered by itself) still sums
  # to all 3 incident persons.
  CDMdbHandler <- .buildSyntheticIncidencePersonCountsHandler()

  result <- getPersonCountsIncidenceFilters(CDMdbHandler, conceptIds = "100SD", sexStratum = 1L)

  ageCounts <- result |>
    dplyr::filter(filter == "age") |>
    dplyr::mutate(stratum = as.integer(stratum), person_counts = as.integer(person_counts)) |>
    dplyr::arrange(stratum) |>
    dplyr::select(stratum, person_counts)

  expected <- tibble::tribble(
    ~stratum, ~person_counts,
    1L, 1L,
    2L, 1L
  )
  ageCounts |> expect_equal(expected)

  visitCounts <- result |>
    dplyr::filter(filter == "visit") |>
    dplyr::mutate(stratum = as.integer(stratum), person_counts = as.integer(person_counts)) |>
    dplyr::arrange(stratum) |>
    dplyr::select(stratum, person_counts)

  # only visit group 0 survives the gender-1 restriction (person 2, visit 7, is gender 2)
  visitCounts |> expect_equal(tibble::tibble(stratum = 0L, person_counts = 2L))

  result |>
    dplyr::filter(filter == "sex") |>
    dplyr::mutate(person_counts = as.integer(person_counts)) |>
    dplyr::pull(person_counts) |>
    sum() |>
    expect_equal(3L)
})

test_that("getPersonCountsIncidenceFilters selected flags mark exactly the passed-in filter values (synthetic)", {
  CDMdbHandler <- .buildSyntheticIncidencePersonCountsHandler()

  result <- getPersonCountsIncidenceFilters(
    CDMdbHandler,
    conceptIds = "100SD",
    sexStratum = 1L,
    yearsRange = c(2011L, 2011L)
  )

  result |>
    dplyr::filter(filter == "sex") |>
    dplyr::arrange(as.integer(stratum)) |>
    dplyr::pull(selected) |>
    expect_equal(c(TRUE, FALSE)) # stratum 1 selected, stratum 2 not

  result |>
    dplyr::filter(filter == "year") |>
    dplyr::arrange(as.integer(stratum)) |>
    dplyr::pull(selected) |>
    expect_equal(c(FALSE, TRUE)) # 2010 not selected, 2011 selected

  result |>
    dplyr::filter(filter %in% c("age", "visit")) |>
    dplyr::pull(selected) |>
    any() |>
    expect_false()
})

test_that("getPersonCountsIncidenceFilters rejects an inverted yearsRange", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCountsIncidenceFilters(CDMdbHandler, conceptIds = "317009SD", yearsRange = c(2020L, 2015L))
  )
})

test_that("getPersonCountsIncidenceFilters returns error if concept id has no descendants", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCountsIncidenceFilters(
      CDMdbHandler,
      conceptIds = "1000000000SD"
    )
  )
})
