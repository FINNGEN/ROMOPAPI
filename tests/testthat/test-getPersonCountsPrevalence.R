# NOTE: these tests read the `stratified_persons` bridge table and the
# `observed_persons_counts_stratified` denominator table. Against
# OnlyCounts-FinnGen they will fail until inst/testdata/data/FinnGenR13_countsOnly.sqlite
# is regenerated via inst/testdata/data/createTestingData.R (requires BigQuery
# access to AtlasDevelopment-full) — see development/STRUCTURE.md.
#
# The FinnGen fixture's stratified_persons is a deterministically bounded SAMPLE
# (helper_createSqliteDatabaseFromDatabase()'s maxPersonsPerConcept cap), while
# observed_persons_counts_stratified is extracted uncapped from the full
# population. Prevalence VALUES computed from the fixture are therefore
# meaningless (the numerator is capped, the denominator is not) — only shape,
# bounds (person_counts <= observed_persons_counts) and cross-getter invariants
# are testable there. Exact arithmetic is covered by the synthetic fixture.

test_that("getPersonCountsPrevalence rejects malformed conceptIds tokens", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(getPersonCountsPrevalence(CDMdbHandler, conceptIds = ""))
  expect_error(getPersonCountsPrevalence(CDMdbHandler, conceptIds = "abc"))
  expect_error(getPersonCountsPrevalence(CDMdbHandler, conceptIds = "317009X"))
  expect_error(getPersonCountsPrevalence(CDMdbHandler, conceptIds = "317009"))
})

test_that("getPersonCountsPrevalence rejects an inverted yearsRange", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCountsPrevalence(CDMdbHandler, conceptIds = "317009SD", yearsRange = c(2020L, 2015L))
  )
})

test_that("getPersonCountsPrevalence works for a single descendant-expanded set", {
  # post-counts test: reads the pre-built stratified_persons / observed_persons_counts_stratified tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsPrevalence_memoise)

  prevalencePersonCounts <- getPersonCountsPrevalence(CDMdbHandler, conceptIds = "317009SD")

  prevalencePersonCounts |>
    colnames() |>
    expect_equal(c("tagged_concept_id", "calendar_year", "person_counts", "observed_persons_counts"))

  prevalencePersonCounts |>
    nrow() |>
    expect_gt(0)

  prevalencePersonCounts |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), is.na)) |>
    nrow() |>
    expect_equal(0)

  # every row is tagged with the requested token
  prevalencePersonCounts |>
    dplyr::pull(tagged_concept_id) |>
    unique() |>
    expect_equal("317009SD")

  # the denominator is always a true population-at-risk: strictly positive,
  # and never smaller than the (capped) numerator
  prevalencePersonCounts |>
    dplyr::pull(observed_persons_counts) |>
    (\(x) all(x > 0))() |>
    expect_true()

  prevalencePersonCounts |>
    dplyr::filter(person_counts > observed_persons_counts) |>
    nrow() |>
    expect_equal(0)
})

test_that("getPersonCountsPrevalence yearsRange restricts the returned years", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsPrevalence_memoise)

  fullPrevalence <- getPersonCountsPrevalence(CDMdbHandler, conceptIds = "317009SD")
  years <- fullPrevalence |> dplyr::pull(calendar_year) |> sort() |> unique()
  skip_if(length(years) < 2, "fixture needs at least 2 distinct years for this test")

  restrictedPrevalence <- getPersonCountsPrevalence(
    CDMdbHandler,
    conceptIds = "317009SD",
    yearsRange = c(years[1], years[1])
  )

  restrictedPrevalence |>
    dplyr::pull(calendar_year) |>
    unique() |>
    expect_equal(years[1])
})

test_that("getPersonCountsPrevalence's per-year numerator matches getPersonCountsFilters' year breakdown", {
  # cross-getter invariant: for a SINGLE token (no pooling across sets) and no
  # sex/age/visit filters, the per-year distinct-person numerator must be
  # identical whichever getter computes it, since both read the same
  # stratified_persons rows with the same (column IN ids) predicate
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsPrevalence_memoise)
  memoise::forget(getPersonCountsFilters_memoise)

  prevalencePersonCounts <- getPersonCountsPrevalence(CDMdbHandler, conceptIds = "317009SD")
  filterPersonCounts <- getPersonCountsFilters(CDMdbHandler, conceptIds = "317009SD")

  yearFilterCounts <- filterPersonCounts |>
    dplyr::filter(filter == "year") |>
    dplyr::select(calendar_year = stratum, person_counts)

  compared <- prevalencePersonCounts |>
    dplyr::select(calendar_year, person_counts) |>
    dplyr::inner_join(yearFilterCounts, by = "calendar_year", suffix = c("_prevalence", "_filters"))

  compared |>
    nrow() |>
    expect_gt(0)

  compared |>
    dplyr::filter(person_counts_prevalence != person_counts_filters) |>
    nrow() |>
    expect_equal(0)
})

test_that("getPersonCountsPrevalence gives exact counts on a hand-built fixture (synthetic)", {
  # Checks the actual numerator/denominator ARITHMETIC against
  # .buildSyntheticPersonCountsHandler() (tests/testthat/helper.R):
  #   100SD (100 + descendant 101, matched via concept_id) -> persons {1,2} in
  #     2010, {3,6} in 2011, none in 2012
  #   200MD (200 + descendant 201, matched via maps_to_concept_id) -> persons
  #     {1,4} in 2010, {6} in 2011, {5} in 2012
  #   observed_persons_counts_stratified: 2010 -> 100+200=300, 2011 -> 50+40=90,
  #     2012 -> 10 (see helper.R for the per-stratum breakdown)
  CDMdbHandler <- .buildSyntheticPersonCountsHandler()

  result <- getPersonCountsPrevalence(CDMdbHandler, conceptIds = "100SD,200MD")

  expected <- tibble::tribble(
    ~tagged_concept_id, ~calendar_year, ~person_counts, ~observed_persons_counts,
    "100SD", 2010L, 2L, 300L,
    "100SD", 2011L, 2L,  90L,
    "100SD", 2012L, 0L,  10L,
    "200MD", 2010L, 2L, 300L,
    "200MD", 2011L, 1L,  90L,
    "200MD", 2012L, 1L,  10L
  )

  result |>
    dplyr::mutate(
      person_counts = as.integer(person_counts),
      observed_persons_counts = as.integer(observed_persons_counts)
    ) |>
    dplyr::arrange(tagged_concept_id, calendar_year) |>
    expect_equal(expected |> dplyr::arrange(tagged_concept_id, calendar_year))
})

test_that("getPersonCountsPrevalence's sexStratum/ageStratum narrow both numerator and denominator (synthetic)", {
  # person 1 (gender 1, age 1) is the only "100SD" person in 2010 under gender_concept_id == 1;
  # filtering to that sex should drop person 2 (gender 2) from the numerator and
  # restrict the denominator to the (2010, gender 1, age 1) cell only (100, not 300)
  CDMdbHandler <- .buildSyntheticPersonCountsHandler()

  result <- getPersonCountsPrevalence(CDMdbHandler, conceptIds = "100SD", sexStratum = 1L)

  result2010 <- result |> dplyr::filter(calendar_year == 2010L)

  result2010 |> dplyr::pull(person_counts) |> expect_equal(1L)
  result2010 |> dplyr::pull(observed_persons_counts) |> expect_equal(100L)
})

test_that("getPersonCountsPrevalence's visitStratum narrows the numerator only (synthetic)", {
  # person 5's event (-> 200MD, 2012) is the only row with a non-zero visit_group_concept_id (5);
  # restricting to that visit group should narrow the numerator but leave the
  # (sex/age-only) denominator unchanged
  CDMdbHandler <- .buildSyntheticPersonCountsHandler()

  unfiltered <- getPersonCountsPrevalence(CDMdbHandler, conceptIds = "200MD") |>
    dplyr::filter(calendar_year == 2012L)
  filtered <- getPersonCountsPrevalence(CDMdbHandler, conceptIds = "200MD", visitStratum = 5L) |>
    dplyr::filter(calendar_year == 2012L)

  unfiltered |> dplyr::pull(person_counts) |> expect_equal(1L)
  filtered |> dplyr::pull(person_counts) |> expect_equal(1L)
  filtered |> dplyr::pull(observed_persons_counts) |>
    expect_equal(unfiltered |> dplyr::pull(observed_persons_counts))

  # a visit group nobody used empties the numerator but not the denominator
  emptied <- getPersonCountsPrevalence(CDMdbHandler, conceptIds = "200MD", visitStratum = 999L) |>
    dplyr::filter(calendar_year == 2012L)
  emptied |> dplyr::pull(person_counts) |> expect_equal(0L)
  emptied |> dplyr::pull(observed_persons_counts) |>
    expect_equal(unfiltered |> dplyr::pull(observed_persons_counts))
})

test_that("getPersonCountsPrevalence returns error if concept id has no descendants", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCountsPrevalence(
      CDMdbHandler,
      conceptIds = "1000000000SD"
    )
  )
})
