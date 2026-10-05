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

# Builds a small synthetic CDMdbHandler (throwaway SQLite file) carrying a
# `stratified_measurements` table whose soft-MAD bins can be computed by hand —
# used to test getMeasurementValueHistogram()'s arithmetic independently of any
# real CDM. 9000001/9000002 are deliberately fake ids that collide with nothing in
# the real vocabulary; 317009 is a real Condition, so the domain guard has something
# to reject.
#
# The 9000001 / unit 8840 partition is one partition of 16 values: 1..11, the two
# extremes (-100, 500) and the three 2020 rows (5, 6, 7). Its median is 6 and the
# median of the absolute deviations from it is 2.5, so the robust range is
# 6 +/- 7*2.5 = [-11.5, 23.5]. The two extremes sit outside it and must land in the
# underflow/overflow bins WITHOUT widening the in-range bins — that is the whole
# point of using MAD rather than min/max, which would have given [-100, 500].
.buildSyntheticMeasurementHandler <- function() {
  dbPath <- tempfile(fileext = ".sqlite")
  withr::defer_parent(unlink(dbPath))

  config <- list(
    database = list(
      databaseId = "SYNM",
      databaseName = "SyntheticMeasurements",
      databaseDescription = "Synthetic measurement-histogram fixture"
    ),
    connection = list(connectionDetailsSettings = list(dbms = "sqlite", server = dbPath)),
    cdm = list(cdmDatabaseSchema = "main", vocabularyDatabaseSchema = "main", resultsDatabaseSchema = "main")
  )
  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(config, loadConnectionChecksLevel = "basicChecks")

  connection <- CDMdbHandler$connectionHandler$getConnection()

  concept <- tibble::tribble(
    ~concept_id, ~concept_name, ~domain_id, ~vocabulary_id, ~concept_class_id, ~standard_concept, ~concept_code,
    9000001L, "Synthetic measurement", "Measurement", "LOINC", "Lab Test", "S", "SYN-MEAS",
    9000002L, "Synthetic child measurement", "Measurement", "LOINC", "Lab Test", "S", "SYN-MEAS-CHILD",
    317009L, "Asthma", "Condition", "SNOMED", "Clinical Finding", "S", "195967001",
    8840L, "milligram per deciliter", "Unit", "UCUM", "Unit", "S", "mg/dL",
    8753L, "millimole per liter", "Unit", "UCUM", "Unit", "S", "mmol/L"
  )
  connection |> DatabaseConnector::insertTable(
    tableName = "concept", data = as.data.frame(concept),
    dropTableIfExists = TRUE, createTable = TRUE, tempTable = FALSE
  )

  # 9000001 has one descendant (9000002), recorded in a DIFFERENT unit — so a
  # "D" token must return two histograms, one per unit, rather than pooling them.
  conceptAncestor <- tibble::tribble(
    ~ancestor_concept_id, ~descendant_concept_id,
    9000001L, 9000001L,
    9000001L, 9000002L,
    9000002L, 9000002L,
    317009L, 317009L
  )
  connection |> DatabaseConnector::insertTable(
    tableName = "concept_ancestor", data = as.data.frame(conceptAncestor),
    dropTableIfExists = TRUE, createTable = TRUE, tempTable = FALSE
  )

  mkRows <- function(conceptId, unitConceptId, values, gender = 8507L, ageDecile = 5L,
                     year = 2010L, visitGroup = 0L) {
    tibble::tibble(
      concept_id = as.integer(conceptId),
      maps_to_concept_id = as.integer(conceptId),
      visit_group_concept_id = as.integer(visitGroup),
      calendar_year = as.integer(year),
      gender_concept_id = as.integer(gender),
      age_decile = as.integer(ageDecile),
      unit_concept_id = as.integer(unitConceptId),
      value_as_number = as.numeric(values)
    )
  }

  stratifiedMeasurements <- dplyr::bind_rows(
    # median 6, MAD 3 -> breaks [-15, 27]; plus two outliers outside that range
    mkRows(9000001L, 8840L, 1:11),
    mkRows(9000001L, 8840L, c(-100, 500)),
    # a second sex/year so the strata filters have something to bite on
    mkRows(9000001L, 8840L, c(5, 6, 7), gender = 8532L, year = 2020L),
    # the descendant, in a different unit -> its own histogram under a D token
    mkRows(9000002L, 8753L, c(10, 20, 30, 40, 50)),
    # all-identical values -> MAD 0 -> soft_min_step reset path
    mkRows(9000002L, 8840L, rep(42, 6))
  )
  connection |> DatabaseConnector::insertTable(
    tableName = "stratified_measurements", data = as.data.frame(stratifiedMeasurements),
    dropTableIfExists = TRUE, createTable = TRUE, tempTable = FALSE
  )

  CDMdbHandler
}
