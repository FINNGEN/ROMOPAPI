test_that("createStratifiedMeasurementsTable works", {
  # counts-creation test: needs a raw OMOP CDM
  skip_if_not(testingDatabase %in% creationDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(
    test_cohortTableHandlerConfig,
    loadConnectionChecksLevel = "basicChecks"
  )
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  stratifiedMeasurementsTable <- "stratified_measurements_test0"
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0(
      "DROP TABLE ",
      resultsDatabaseSchema,
      ".",
      stratifiedMeasurementsTable
    ))
  })

  suppressWarnings(
    createStratifiedMeasurementsTable(
      CDMdbHandler,
      stratifiedMeasurementsTable = stratifiedMeasurementsTable,
      visitSourceGroupConceptIds = test_visitSourceGroupConceptIds
    )
  )

  stratifiedMeasurements <- CDMdbHandler$connectionHandler$tbl(I(paste0(
    resultsDatabaseSchema,
    ".",
    stratifiedMeasurementsTable
  )))

  stratifiedMeasurements |>
    head() |>
    dplyr::collect() |>
    colnames() |>
    expect_equal(c(
      "concept_id",
      "maps_to_concept_id",
      "visit_group_concept_id",
      "calendar_year",
      "gender_concept_id",
      "age_decile",
      "unit_concept_id",
      "value_as_number"
    ))

  # the table only exists to carry values, so a NULL value_as_number is never useful
  stratifiedMeasurements |>
    dplyr::filter(is.na(value_as_number)) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

  # How many rows the table holds is database-dependent, so assert the relationship
  # to the source rather than a fixed count. Eunomia-GiBleed is the case that forces
  # this: it has ~44k measurement rows but *every* value_as_number is NULL, so the
  # table is legitimately empty there, while AtlasDevelopment-5k has real values.
  builtRows <- stratifiedMeasurements |>
    dplyr::count() |>
    dplyr::pull(n)

  sourceRowsWithValue <- CDMdbHandler$connectionHandler$tbl(I(paste0(
    CDMdbHandler$cdmDatabaseSchema,
    ".measurement"
  ))) |>
    dplyr::filter(!is.na(value_as_number)) |>
    dplyr::count() |>
    dplyr::pull(n)

  # the table additionally requires a valid observation period and a known concept,
  # so it can only ever be a subset of the measurements that carry a value
  expect_lte(builtRows, sourceRowsWithValue)

  # but when the CDM does have usable values, they must not all be dropped
  if (sourceRowsWithValue > 0) {
    expect_gt(builtRows, 0)
  }
})
