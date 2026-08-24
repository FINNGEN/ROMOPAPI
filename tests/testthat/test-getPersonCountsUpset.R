# NOTE: these tests read the `stratified_persons` bridge table and the
# `person_counts`/`descendant_person_counts` columns of `code_counts`. Against
# OnlyCounts-FinnGen they will fail until inst/testdata/data/FinnGenR13_countsOnly.sqlite
# is regenerated via inst/testdata/data/createTestingData.R (requires BigQuery
# access to AtlasDevelopment-full) — see development/STRUCTURE.md.
#
# The FinnGen fixture's stratified_persons is a deterministically bounded SAMPLE
# (helper_createSqliteDatabaseFromDatabase()'s maxPersonsPerConcept cap — real-world
# codes like "Asthma" touch a large fraction of the ~500k FinnGen population, and
# the full bridge is far too large to ship as a git-committed fixture). So counts
# derived from it are bounded by, but generally don't equal, the true
# person_counts/descendant_person_counts in code_counts (built from the
# uncapped source). Bridge-vs-bridge invariants (getPersonCountsFilters vs
# getPersonCountsUpset, both from the same capped data) stay exact; the
# exact-equality invariant against code_counts is fully covered elsewhere by the
# Eunomia-GiBleed and AtlasDevelopment-5k creation-time tests, which build the
# bridge uncapped from the real CDM.

test_that("getPersonCountsUpset rejects malformed conceptIds tokens", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(getPersonCountsUpset(CDMdbHandler, conceptIds = ""))
  expect_error(getPersonCountsUpset(CDMdbHandler, conceptIds = "abc"))
  expect_error(getPersonCountsUpset(CDMdbHandler, conceptIds = "317009X"))
  expect_error(getPersonCountsUpset(CDMdbHandler, conceptIds = "317009"))
})

test_that("getPersonCountsUpset works for a single descendant-expanded set", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsUpset_memoise)

  upsetPersonCounts <- getPersonCountsUpset(CDMdbHandler, conceptIds = "317009SD")

  upsetPersonCounts |>
    colnames() |>
    expect_equal(c("group", "person_counts"))

  upsetPersonCounts |>
    dplyr::filter(is.na(group) | is.na(person_counts)) |>
    nrow() |>
    expect_equal(0)

  # a single set has exactly one exclusive region, labeled with its own token
  upsetPersonCounts |>
    dplyr::pull(group) |>
    expect_equal("317009SD")

  # bounded-sample invariant: capped bridge persons for the concept + descendants
  # can't exceed the (uncapped) descendant_person_counts in code_counts
  codeCounts <- CDMdbHandler$connectionHandler$tbl(I(paste0(CDMdbHandler$resultsDatabaseSchema, ".code_counts"))) |>
    dplyr::filter(concept_id == 317009) |>
    dplyr::collect()

  totalUpsetPersons <- upsetPersonCounts |> dplyr::pull(person_counts) |> sum()
  totalUpsetPersons |> expect_gt(0)
  totalUpsetPersons |> expect_lte(codeCounts$descendant_person_counts)
})

test_that("getPersonCountsUpset without D restricts to the exact code", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsUpset_memoise)

  upsetPersonCounts <- getPersonCountsUpset(CDMdbHandler, conceptIds = "317009S")

  upsetPersonCounts |>
    dplyr::pull(group) |>
    expect_equal("317009S")

  codeCounts <- CDMdbHandler$connectionHandler$tbl(I(paste0(CDMdbHandler$resultsDatabaseSchema, ".code_counts"))) |>
    dplyr::filter(concept_id == 317009) |>
    dplyr::collect()

  rootUpsetPersons <- upsetPersonCounts |> dplyr::pull(person_counts)
  rootUpsetPersons |> expect_gt(0)
  rootUpsetPersons |> expect_lte(codeCounts$person_counts)
})

test_that("getPersonCountsUpset keeps tokens with the same concept id but different tags distinct", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsUpset_memoise)

  upsetPersonCounts <- getPersonCountsUpset(CDMdbHandler, conceptIds = "317009S,317009SD")

  # SD is self-inclusive, so S is always a subset of SD: every exclusive region
  # must include "317009SD" — a bare "317009S" region (without SD) is impossible.
  groups <- upsetPersonCounts |> dplyr::pull(group)
  groups |> grepl(pattern = "317009SD", fixed = TRUE) |> all() |> expect_true()
  groups |> (\(x) "317009S" %in% x)() |> expect_false()
})

test_that("getPersonCountsUpset gives exact overlapping-region counts on a hand-built fixture (synthetic)", {
  # Checks the actual exclusive-region ARITHMETIC, not just shape/bounds, against
  # .buildSyntheticPersonCountsHandler() (tests/testthat/helper.R): 6 persons,
  # where persons 1 and 6 each carry an event under BOTH concept trees (100 and
  # 200), so "100SD"/"200MD" genuinely overlap instead of only ever containing
  # disjoint singletons.
  #   100SD (100 + descendant 101) -> persons {1,2,3,6}
  #   200MD (200 + descendant 201, via maps_to_concept_id) -> persons {1,4,5,6}
  #   overlap {1,6}; 100SD-only {2,3}; 200MD-only {4,5}
  CDMdbHandler <- .buildSyntheticPersonCountsHandler()

  result <- getPersonCountsUpset(CDMdbHandler, conceptIds = "100SD,200MD")

  expected <- tibble::tribble(
    ~group, ~person_counts,
    "100SD-200MD", 2L,
    "100SD", 2L,
    "200MD", 2L
  )

  result |>
    dplyr::mutate(person_counts = as.integer(person_counts)) |>
    dplyr::arrange(group) |>
    expect_equal(expected |> dplyr::arrange(group))
})

test_that("getPersonCountsUpset gives exact self-inclusive overlap counts on a hand-built fixture (synthetic)", {
  # 100S (concept 100 only) -> persons {1,6}; 100SD (100 + descendant 101) ->
  # persons {1,2,3,6}. SD is self-inclusive, so 100S is always a subset of
  # 100SD: the exclusive regions are the shared root {1,6} and the
  # descendants-only remainder {2,3} — a 2-person overlap, not a singleton.
  CDMdbHandler <- .buildSyntheticPersonCountsHandler()

  result <- getPersonCountsUpset(CDMdbHandler, conceptIds = "100S,100SD")

  expected <- tibble::tribble(
    ~group, ~person_counts,
    "100S-100SD", 2L,
    "100SD", 2L
  )

  result |>
    dplyr::mutate(person_counts = as.integer(person_counts)) |>
    dplyr::arrange(group) |>
    expect_equal(expected |> dplyr::arrange(group))
})

test_that("getPersonCountsUpset stratum filters narrow totals consistently with getPersonCountsFilters", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getPersonCountsUpset_memoise)
  memoise::forget(getPersonCountsFilters_memoise)

  fullUpset <- getPersonCountsUpset(CDMdbHandler, conceptIds = "317009SD")
  filterPersonCounts <- getPersonCountsFilters(CDMdbHandler, conceptIds = "317009SD")
  sexStrata <- filterPersonCounts |>
    dplyr::filter(filter == "sex") |>
    dplyr::pull(stratum)

  # filtering by every sex stratum present recovers the unfiltered total
  filtered <- getPersonCountsUpset(CDMdbHandler, conceptIds = "317009SD", sexStratum = sexStrata)
  filtered |>
    dplyr::pull(person_counts) |>
    sum() |>
    expect_equal(fullUpset |> dplyr::pull(person_counts) |> sum())

  # filtering by a single sex stratum matches its getPersonCountsFilters row
  # (that row is computed with no sex filter of its own applied, i.e. the full total)
  oneSex <- getPersonCountsUpset(CDMdbHandler, conceptIds = "317009SD", sexStratum = sexStrata[1])
  oneSex |>
    dplyr::pull(person_counts) |>
    sum() |>
    expect_equal(filterPersonCounts |> dplyr::filter(filter == "sex", stratum == sexStrata[1]) |> dplyr::pull(person_counts))
})

test_that("getPersonCountsUpset rejects an inverted yearsRange", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCountsUpset(CDMdbHandler, conceptIds = "317009SD", yearsRange = c(2020L, 2015L))
  )
})

test_that("getPersonCountsUpset returns error if concept id has no descendants", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCountsUpset(
      CDMdbHandler,
      conceptIds = "1000000000SD"
    )
  )
})
