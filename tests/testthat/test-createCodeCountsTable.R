test_that("createAtomicCodeCountsTable works with duplicated counts", {
  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler$finalize()
  })

  codeAtomicCountsWithDuplicatedCountsTable <- "code_atomic_counts_test0"
  codeAtomicCountsTable <- "code_atomic_counts_test1"

  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0("DROP TABLE ", resultsDatabaseSchema, ".", codeAtomicCountsWithDuplicatedCountsTable))
    CDMdbHandler$connectionHandler$executeSql(paste0("DROP TABLE ", resultsDatabaseSchema, ".", codeAtomicCountsTable))
  })

  # codeAtomicCountsWithDuplicatedCounts
  domain <- tibble::tribble(
    ~domain_id, ~table_name, ~concept_id_field, ~date_field, ~maps_to_concept_id_field,
    "Condition", "condition_occurrence", "condition_source_concept_id", "condition_start_date", "condition_source_concept_id",
  )
  suppressWarnings(
    createAtomicCodeCountsTable(CDMdbHandler, domains = domain, codeAtomicCountsTable = codeAtomicCountsWithDuplicatedCountsTable)
  )
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema
  codeAtomicCountsWithDuplicatedCounts <- CDMdbHandler$connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", codeAtomicCountsWithDuplicatedCountsTable))

  # codeAtomicCounts
  domain <- tibble::tribble(
    ~domain_id, ~table_name, ~concept_id_field, ~date_field, ~maps_to_concept_id_field,
    "Condition", "condition_occurrence", "condition_source_concept_id", "condition_start_date", "condition_concept_id",
  )
  createAtomicCodeCountsTable(CDMdbHandler, domains = domain, codeAtomicCountsTable = codeAtomicCountsTable)
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema
  codeAtomicCounts <- CDMdbHandler$connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", codeAtomicCountsTable))

  # test counts of codeAtomicCounts Distinct by maps_to_concept_id is the same as codeAtomicCountsWithDuplicatedCounts
  n <- codeAtomicCountsWithDuplicatedCounts |>
    dplyr::count() |>
    dplyr::pull(n)
  n2 <- codeAtomicCounts |>
    dplyr::distinct(concept_id, calendar_year, gender_concept_id, age_decile, event_counts) |>
    dplyr::count() |>
    dplyr::pull(n)

  n |> expect_equal(n2)

  # test that all the duplicated counts in codeAtomicCountsWithDuplicatedCounts are split into the codeAtomicCounts table
  dplyr::inner_join(
    codeAtomicCountsWithDuplicatedCounts |>
      dplyr::group_by(concept_id) |>
      dplyr::summarise(event_counts = sum(event_counts), .groups = "drop") |>
      dplyr::distinct(concept_id, event_counts) |> 
      dplyr::rename(event_counts_duplicated = event_counts),
    codeAtomicCounts |>
      dplyr::group_by(concept_id) |>
      dplyr::summarise(event_counts = sum(event_counts), .groups = "drop") |>
      dplyr::distinct(concept_id, event_counts) |> 
      dplyr::rename(event_counts_no_duplicated = event_counts),
    by = c("concept_id")
  ) |>
    dplyr::filter(event_counts_duplicated != event_counts_no_duplicated)  |> 
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

})

test_that("createAtomicCodeCountsTable works ", {
  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler$finalize()
  })

  codeAtomicCountsTable <- "code_atomic_counts_test2"
  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0("DROP TABLE ", resultsDatabaseSchema, ".", codeAtomicCountsTable))
  })

  createAtomicCodeCountsTable(CDMdbHandler, codeAtomicCountsTable = codeAtomicCountsTable)
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema
  codeAtomicCounts <- CDMdbHandler$connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", codeAtomicCountsTable))

  codeAtomicCounts |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

})



test_that("createObservationCountsTable works", {
  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler$finalize()
  })

  connection <- CDMdbHandler$connectionHandler$getConnection()
  cdmDatabaseSchema <- CDMdbHandler$cdmDatabaseSchema
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

  sqlPath <- system.file("sql", "sql_server", "createObservationCountsTable.sql", package = "ROMOPAPI")
  sql <- SqlRender::readSql(sqlPath)
  sql <- SqlRender::render(sql,
    cdmDatabaseSchema = cdmDatabaseSchema,
    resultsDatabaseSchema = resultsDatabaseSchema
  )
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)

  DatabaseConnector::executeSql(connection, sql)

  observation_counts <- CDMdbHandler$connectionHandler$tbl(paste0(resultsDatabaseSchema, ".observation_counts"))
  observation_counts |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)
})


test_that("createCodeCountsTable works", {
  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler$finalize()
  })

  codeCountsTable <- "code_counts_test0"
  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0("DROP TABLE ", resultsDatabaseSchema, ".", codeCountsTable))
  })

  createCodeCountsTable(CDMdbHandler, codeCountsTable = codeCountsTable)

  # - Check if the table was created
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema
  cdmDatabaseSchema <- CDMdbHandler$cdmDatabaseSchema
  code_counts <- CDMdbHandler$connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", codeCountsTable))

  code_counts |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)
  code_counts |>
    head() |>
    dplyr::collect() |>
    colnames() |>
    expect_equal(c(
      "concept_id",
      "calendar_year", 
      "gender_concept_id", 
      "age_decile",
      "event_counts", 
      "descendant_event_counts"
    ))
  code_counts |>
    dplyr::filter(descendant_event_counts < event_counts) |> 
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

})
