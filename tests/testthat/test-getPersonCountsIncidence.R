# NOTE: these tests read the `stratified_persons` bridge table and the
# `observed_persons_counts_stratified` denominator table — both already shipped
# in inst/testdata/data/FinnGenR13_countsOnly.sqlite as of the Prevalence PR,
# so no fixture regeneration is needed for the fixture-based tests below.
#
# Prevalence's caveat still applies: the FinnGen fixture's stratified_persons is
# a deterministically bounded SAMPLE (maxPersonsPerConcept cap), while
# observed_persons_counts_stratified is extracted uncapped — only shape, bounds
# and cross-getter invariants are testable there. Exact arithmetic is covered by
# .buildSyntheticIncidencePersonCountsHandler() (tests/testthat/helper.R).

test_that("getPersonCountsIncidence rejects malformed conceptIds tokens", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(getPersonCountsIncidence(CDMdbHandler, conceptIds = ""))
  expect_error(getPersonCountsIncidence(CDMdbHandler, conceptIds = "abc"))
  expect_error(getPersonCountsIncidence(CDMdbHandler, conceptIds = "317009X"))
  expect_error(getPersonCountsIncidence(CDMdbHandler, conceptIds = "317009"))
})

test_that("getPersonCountsIncidence rejects an inverted yearsRange", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCountsIncidence(CDMdbHandler, conceptIds = "317009SD", yearsRange = c(2020L, 2015L))
  )
})

test_that("getPersonCountsIncidence works for a single descendant-expanded set", {
  # post-counts test: reads the pre-built stratified_persons / observed_persons_counts_stratified tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsIncidence_memoise)

  incidencePersonCounts <- getPersonCountsIncidence(CDMdbHandler, conceptIds = "317009SD")

  incidencePersonCounts |>
    colnames() |>
    expect_equal(c("tagged_concept_id", "calendar_year", "person_counts", "observed_persons_counts"))

  incidencePersonCounts |>
    nrow() |>
    expect_gt(0)

  incidencePersonCounts |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), is.na)) |>
    nrow() |>
    expect_equal(0)

  incidencePersonCounts |>
    dplyr::pull(tagged_concept_id) |>
    unique() |>
    expect_equal("317009SD")

  incidencePersonCounts |>
    dplyr::pull(observed_persons_counts) |>
    (\(x) all(x > 0))() |>
    expect_true()

  incidencePersonCounts |>
    dplyr::filter(person_counts > observed_persons_counts) |>
    nrow() |>
    expect_equal(0)
})

test_that("getPersonCountsIncidence's per-year total never exceeds getPersonCountsPrevalence's", {
  # invariant: incidence counts each person at most once ever (their first year),
  # prevalence can count them again in later years -- so summed over all years,
  # incidence <= prevalence for the same token.
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsIncidence_memoise)
  memoise::forget(getPersonCountsPrevalence_memoise)

  incidenceTotal <- getPersonCountsIncidence(CDMdbHandler, conceptIds = "317009SD") |>
    dplyr::pull(person_counts) |>
    sum()
  prevalenceTotal <- getPersonCountsPrevalence(CDMdbHandler, conceptIds = "317009SD") |>
    dplyr::pull(person_counts) |>
    sum()

  incidenceTotal |> expect_lte(prevalenceTotal)
})

test_that("getPersonCountsIncidence gives exact counts on a hand-built fixture (synthetic)", {
  # Checks the actual numerator/denominator ARITHMETIC against
  # .buildSyntheticIncidencePersonCountsHandler() (tests/testthat/helper.R):
  #   "100SD" -> person 1's first-ever record is 2010 (their 2012 record is a
  #   SECOND occurrence and must not count); persons 2 and 3 are each incident
  #   in 2011.
  CDMdbHandler <- .buildSyntheticIncidencePersonCountsHandler()

  result <- getPersonCountsIncidence(CDMdbHandler, conceptIds = "100SD")

  expected <- tibble::tribble(
    ~tagged_concept_id, ~calendar_year, ~person_counts, ~observed_persons_counts,
    "100SD", 2010L, 1L, 100L,
    "100SD", 2011L, 2L,  90L,
    "100SD", 2012L, 0L,  10L
  )

  result |>
    dplyr::mutate(
      person_counts = as.integer(person_counts),
      observed_persons_counts = as.integer(observed_persons_counts)
    ) |>
    dplyr::arrange(calendar_year) |>
    expect_equal(expected |> dplyr::arrange(calendar_year))
})

test_that("getPersonCountsIncidence's yearsRange can legitimately zero out a token (synthetic)", {
  # person 1's only incident year is 2010 -- restricting to 2012 (where they DO
  # have a later, non-incident record) must return a zero count, not that record.
  CDMdbHandler <- .buildSyntheticIncidencePersonCountsHandler()

  restricted <- getPersonCountsIncidence(CDMdbHandler, conceptIds = "100SD", yearsRange = c(2012L, 2012L))

  restricted |> dplyr::pull(calendar_year) |> expect_equal(2012L)
  restricted |> dplyr::pull(person_counts) |> expect_equal(0L)
  restricted |> dplyr::pull(observed_persons_counts) |> expect_equal(10L)
})

test_that("getPersonCountsIncidence's sexStratum narrows both numerator and denominator (synthetic)", {
  # in 2011, "100SD" is incident for person 2 (gender 2) and person 3 (gender 1).
  # Restricting to gender 1 should drop person 2 from the numerator and restrict
  # the denominator to the (2011, gender 1, age 2) cell only (50, not 90).
  CDMdbHandler <- .buildSyntheticIncidencePersonCountsHandler()

  result <- getPersonCountsIncidence(CDMdbHandler, conceptIds = "100SD", sexStratum = 1L) |>
    dplyr::filter(calendar_year == 2011L)

  result |> dplyr::pull(person_counts) |> expect_equal(1L)
  result |> dplyr::pull(observed_persons_counts) |> expect_equal(50L)
})

test_that("getPersonCountsIncidence's visitStratum narrows the numerator only (synthetic)", {
  # in 2011, only person 2's incident row carries a non-zero visit group (7);
  # restricting to that visit group should narrow the numerator but leave the
  # (sex/age-only) denominator unchanged.
  CDMdbHandler <- .buildSyntheticIncidencePersonCountsHandler()

  unfiltered <- getPersonCountsIncidence(CDMdbHandler, conceptIds = "100SD") |>
    dplyr::filter(calendar_year == 2011L)
  filtered <- getPersonCountsIncidence(CDMdbHandler, conceptIds = "100SD", visitStratum = 7L) |>
    dplyr::filter(calendar_year == 2011L)

  unfiltered |> dplyr::pull(person_counts) |> expect_equal(2L)
  filtered |> dplyr::pull(person_counts) |> expect_equal(1L)
  filtered |> dplyr::pull(observed_persons_counts) |>
    expect_equal(unfiltered |> dplyr::pull(observed_persons_counts))

  # a visit group nobody used empties the numerator but not the denominator
  emptied <- getPersonCountsIncidence(CDMdbHandler, conceptIds = "100SD", visitStratum = 999L) |>
    dplyr::filter(calendar_year == 2011L)
  emptied |> dplyr::pull(person_counts) |> expect_equal(0L)
  emptied |> dplyr::pull(observed_persons_counts) |>
    expect_equal(unfiltered |> dplyr::pull(observed_persons_counts))
})

test_that("getPersonCountsIncidence returns error if concept id has no descendants", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCountsIncidence(
      CDMdbHandler,
      conceptIds = "1000000000SD"
    )
  )
})
