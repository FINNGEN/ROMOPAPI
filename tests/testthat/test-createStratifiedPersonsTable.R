test_that("createStratifiedPersonsTable works", {
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

  stratifiedPersonsTable <- "stratified_persons_test0"
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0(
      "DROP TABLE ",
      resultsDatabaseSchema,
      ".",
      stratifiedPersonsTable
    ))
  })

  domain <- tibble::tribble(
    ~domain_id  , ~table_name            , ~concept_id_field      , ~date_field            , ~maps_to_concept_id_field     ,
    "Condition" , "condition_occurrence" , "condition_concept_id" , "condition_start_date" , "condition_source_concept_id"
  )

  suppressWarnings(
    createStratifiedPersonsTable(
      CDMdbHandler,
      domains = domain,
      stratifiedPersonsTable = stratifiedPersonsTable
    )
  )

  stratifiedPersons <- CDMdbHandler$connectionHandler$tbl(I(paste0(
    resultsDatabaseSchema,
    ".",
    stratifiedPersonsTable
  )))

  stratifiedPersons |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

  stratifiedPersons |>
    head() |>
    dplyr::collect() |>
    colnames() |>
    expect_equal(c(
      "person_id",
      "concept_id",
      "maps_to_concept_id",
      "visit_group_concept_id",
      "calendar_year",
      "gender_concept_id",
      "age_decile"
    ))

  # no duplicate person_id per concept_id/stratum bucket
  stratifiedPersons |>
    dplyr::count(person_id, concept_id, maps_to_concept_id, visit_group_concept_id, calendar_year, gender_concept_id, age_decile) |>
    dplyr::filter(n > 1) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

  stratifiedPersons |>
    dplyr::filter(visit_group_concept_id != 0) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)
})

test_that("createStratifiedPersonsTable works with visit_source_group_concept_ids", {
  # visit-source-group logic needs the FinnGen visit concepts (BigQuery only)
  skip_if(testingDatabase != "AtlasDevelopment-5k")

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(
    test_cohortTableHandlerConfig,
    loadConnectionChecksLevel = "basicChecks"
  )
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  stratifiedPersonsTable <- "stratified_persons_test0"
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0(
      "DROP TABLE ",
      resultsDatabaseSchema,
      ".",
      stratifiedPersonsTable
    ))
  })

  domain <- tibble::tribble(
    ~domain_id  , ~table_name            , ~concept_id_field      , ~date_field            , ~maps_to_concept_id_field     ,
    "Condition" , "condition_occurrence" , "condition_concept_id" , "condition_start_date" , "condition_source_concept_id"
  )

  # database-dependent, from databasesConfig.yml (see setup.R)
  visitSourceGroupConceptIds <- test_visitSourceGroupConceptIds

  suppressWarnings(
    createStratifiedPersonsTable(
      CDMdbHandler,
      domains = domain,
      stratifiedPersonsTable = stratifiedPersonsTable,
      visitSourceGroupConceptIds = visitSourceGroupConceptIds
    )
  )

  stratifiedPersons <- CDMdbHandler$connectionHandler$tbl(I(paste0(
    resultsDatabaseSchema,
    ".",
    stratifiedPersonsTable
  )))

  stratifiedPersons |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

  stratifiedPersons |>
    dplyr::filter(visit_group_concept_id == 0) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

  stratifiedPersons |>
    dplyr::distinct(visit_group_concept_id) |>
    dplyr::pull(visit_group_concept_id) |>
    (\(x) expect_true(all(x %in% visitSourceGroupConceptIds)))()
})
