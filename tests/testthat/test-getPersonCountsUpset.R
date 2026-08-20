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

test_that("getPersonCountsUpset works", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getConceptTree_memoise)
  memoise::forget(getPersonCountsUpset_memoise)

  upsetPersonCounts <- getPersonCountsUpset(CDMdbHandler, conceptId = 317009L)

  upsetPersonCounts |>
    colnames() |>
    expect_equal(c("group", "person_counts"))

  upsetPersonCounts |>
    dplyr::filter(is.na(group) | is.na(person_counts)) |>
    nrow() |>
    expect_equal(0)

  # bounded-sample invariant: the exclusive UpSet regions partition the tree's
  # (capped) persons, so they sum to at most the root's descendant_person_counts
  # in code_counts, and are non-empty when the fixture has any bridge rows at all
  codeCounts <- CDMdbHandler$connectionHandler$tbl(I(paste0(CDMdbHandler$resultsDatabaseSchema, ".code_counts"))) |>
    dplyr::filter(concept_id == 317009) |>
    dplyr::collect()

  totalUpsetPersons <- upsetPersonCounts |> dplyr::pull(person_counts) |> sum()
  totalUpsetPersons |> expect_gt(0)
  totalUpsetPersons |> expect_lte(codeCounts$descendant_person_counts)
})

test_that("getPersonCountsUpset level cutoff restricts to the root", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getConceptTree_memoise)
  memoise::forget(getPersonCountsUpset_memoise)

  upsetPersonCounts <- getPersonCountsUpset(CDMdbHandler, conceptId = 317009L, level = 0L)

  upsetPersonCounts |>
    dplyr::pull(group) |>
    expect_equal("317009")

  codeCounts <- CDMdbHandler$connectionHandler$tbl(I(paste0(CDMdbHandler$resultsDatabaseSchema, ".code_counts"))) |>
    dplyr::filter(concept_id == 317009) |>
    dplyr::collect()

  rootUpsetPersons <- upsetPersonCounts |> dplyr::pull(person_counts)
  rootUpsetPersons |> expect_gt(0)
  rootUpsetPersons |> expect_lte(codeCounts$person_counts)
})

test_that("getPersonCountsUpset stratum filters narrow totals consistently with getPersonCountsFilters", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getConceptTree_memoise)
  memoise::forget(getPersonCountsUpset_memoise)
  memoise::forget(getPersonCountsFilters_memoise)

  fullUpset <- getPersonCountsUpset(CDMdbHandler, conceptId = 317009L)
  filterPersonCounts <- getPersonCountsFilters(CDMdbHandler, conceptId = 317009L)
  sexStrata <- filterPersonCounts |>
    dplyr::filter(filter == "sex") |>
    dplyr::pull(stratum)

  # filtering by every sex stratum present recovers the unfiltered total
  filtered <- getPersonCountsUpset(CDMdbHandler, conceptId = 317009L, sexStratum = sexStrata)
  filtered |>
    dplyr::pull(person_counts) |>
    sum() |>
    expect_equal(fullUpset |> dplyr::pull(person_counts) |> sum())

  # filtering by a single sex stratum matches its getPersonCountsFilters row
  oneSex <- getPersonCountsUpset(CDMdbHandler, conceptId = 317009L, sexStratum = sexStrata[1])
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
    getPersonCountsUpset(CDMdbHandler, conceptId = 317009L, yearsRange = c(2020L, 2015L))
  )
})

test_that("getPersonCountsUpset returns error if conceptId is not found", {
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
      conceptId = c(1000000000)
    )
  )
})
