# Builds a small synthetic CDMdbHandler (throwaway SQLite file) with hand-picked
# concept_ancestor / stratified_persons rows whose correct getPersonCountsUpset /
# getPersonCountsFilters output can be computed by hand — used to test the actual
# arithmetic, not just shape/invariants, independent of the real FinnGen fixture.
# Two persons (1 and 6) carry events under BOTH concept trees, so exclusive-region
# math actually has to combine overlapping patients rather than only ever seeing
# one patient per set. See the "(synthetic)" tests in test-getPersonCountsUpset.R
# and test-getPersonCountsFilters.R for the dataset and the hand-computed values.
.buildSyntheticPersonCountsHandler <- function() {
  dbPath <- tempfile(fileext = ".sqlite")
  withr::defer_parent(unlink(dbPath))

  config <- list(
    database = list(
      databaseId = "SYN",
      databaseName = "Synthetic",
      databaseDescription = "Synthetic person-counts fixture"
    ),
    connection = list(connectionDetailsSettings = list(dbms = "sqlite", server = dbPath)),
    cdm = list(cdmDatabaseSchema = "main", vocabularyDatabaseSchema = "main", resultsDatabaseSchema = "main")
  )
  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(config, loadConnectionChecksLevel = "basicChecks")

  connection <- CDMdbHandler$connectionHandler$getConnection()

  # concept 100 has one descendant (101); concept 200 has one descendant (201).
  # concept_ancestor always includes the ancestor-equals-descendant self row.
  conceptAncestor <- tibble::tribble(
    ~ancestor_concept_id, ~descendant_concept_id,
    100L, 100L,
    100L, 101L,
    101L, 101L,
    200L, 200L,
    200L, 201L
  )
  connection |> DatabaseConnector::insertTable(
    tableName = "concept_ancestor",
    data = conceptAncestor,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE
  )

  # 6 persons, 8 rows (one row per person per event). Standard-concept events are
  # self-mapped (maps_to_concept_id == concept_id); non-standard source concepts
  # (9999/9998) map to standard concepts 200/201. Persons 1 and 6 each carry TWO
  # events — one under the 100-tree, one under the 200-tree — so they land in
  # BOTH "100SD" and "200MD", giving getPersonCountsUpset a real 2-way overlap
  # region instead of only disjoint singletons. A person's rows share the same
  # stratum (sex/age/visit/year) so the getPersonCountsFilters ground truth
  # stays simple to hand-compute.
  stratifiedPersons <- tibble::tribble(
    ~person_id, ~concept_id, ~maps_to_concept_id, ~visit_group_concept_id, ~calendar_year, ~gender_concept_id, ~age_decile,
    1L,  100L,  100L,  0L, 2010L, 1L, 1L, # person 1, event on concept 100
    1L, 9999L,  200L,  0L, 2010L, 1L, 1L, # person 1, ALSO an event mapping to 200
    2L,  101L,  101L,  0L, 2010L, 2L, 1L,
    3L,  101L,  101L,  0L, 2011L, 1L, 2L,
    4L, 9999L,  200L,  0L, 2010L, 2L, 1L,
    5L, 9998L,  201L,  5L, 2012L, 1L, 3L,
    6L,  100L,  100L,  0L, 2011L, 2L, 2L, # person 6, event on concept 100
    6L, 9998L,  201L,  0L, 2011L, 2L, 2L  # person 6, ALSO an event mapping to 201
  )
  connection |> DatabaseConnector::insertTable(
    tableName = "stratified_persons",
    data = stratifiedPersons,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE
  )

  CDMdbHandler
}
