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
# uncapped source). Bridge-vs-bridge invariants (filter_person_counts vs
# upset_person_counts, both from the same capped data) stay exact; the
# exact-equality invariant against code_counts is fully covered elsewhere by the
# Eunomia-GiBleed and AtlasDevelopment-5k creation-time tests, which build the
# bridge uncapped from the real CDM.

test_that("getPersonCounts works", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getConceptTree_memoise)
  memoise::forget(getPersonCounts_memoise)

  result <- getPersonCounts(CDMdbHandler, conceptId = 317009L)

  result |>
    names() |>
    expect_equal(c("filter_person_counts", "upset_person_counts"))

  #
  # filter_person_counts
  #

  filterPersonCounts <- result$filter_person_counts

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

  #
  # upset_person_counts
  #

  upsetPersonCounts <- result$upset_person_counts

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

test_that("getPersonCounts level cutoff restricts upset_person_counts to the root", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getConceptTree_memoise)
  memoise::forget(getPersonCounts_memoise)

  result <- getPersonCounts(CDMdbHandler, conceptId = 317009L, level = 0L)

  result$upset_person_counts |>
    dplyr::pull(group) |>
    expect_equal("317009")

  codeCounts <- CDMdbHandler$connectionHandler$tbl(I(paste0(CDMdbHandler$resultsDatabaseSchema, ".code_counts"))) |>
    dplyr::filter(concept_id == 317009) |>
    dplyr::collect()

  rootUpsetPersons <- result$upset_person_counts |> dplyr::pull(person_counts)
  rootUpsetPersons |> expect_gt(0)
  rootUpsetPersons |> expect_lte(codeCounts$person_counts)
})

test_that("getPersonCounts stratum filters narrow upset_person_counts consistently with filter_person_counts", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getConceptTree_memoise)
  memoise::forget(getPersonCounts_memoise)

  full <- getPersonCounts(CDMdbHandler, conceptId = 317009L)
  sexStrata <- full$filter_person_counts |>
    dplyr::filter(filter == "sex") |>
    dplyr::pull(stratum)

  # filtering by every sex stratum present recovers the unfiltered total
  filtered <- getPersonCounts(CDMdbHandler, conceptId = 317009L, sexStratum = sexStrata)
  filtered$upset_person_counts |>
    dplyr::pull(person_counts) |>
    sum() |>
    expect_equal(full$upset_person_counts |> dplyr::pull(person_counts) |> sum())

  # filtering by a single sex stratum matches its filter_person_counts row
  oneSex <- getPersonCounts(CDMdbHandler, conceptId = 317009L, sexStratum = sexStrata[1])
  oneSex$upset_person_counts |>
    dplyr::pull(person_counts) |>
    sum() |>
    expect_equal(full$filter_person_counts |> dplyr::filter(filter == "sex", stratum == sexStrata[1]) |> dplyr::pull(person_counts))
})

test_that("getPersonCounts returns error if conceptId is not found", {
  # post-counts test: reads the pre-built code_counts / stratified_persons tables
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  expect_error(
    getPersonCounts(
      CDMdbHandler,
      conceptId = c(1000000000)
    )
  )
})
