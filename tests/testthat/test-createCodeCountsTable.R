test_that("createStratifiedCodeCountsTable works with duplicated counts", {
  # only works in a full CDM database
  skip_if(testingDatabase == "OnlyCounts-FinnGen")

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  stratifiedCodeCountsTable <- "stratified_code_counts_test0"
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0("DROP TABLE ", resultsDatabaseSchema, ".", stratifiedCodeCountsTable))
  })

  domain  <- tibble::tribble(
    ~domain_id, ~table_name, ~concept_id_field, ~date_field, ~maps_to_concept_id_field,
    "Condition", "condition_occurrence", "condition_concept_id", "condition_start_date", "condition_source_concept_id"
  )

  # codeAtomicCountsWithDuplicatedCounts
  suppressWarnings(
    createStratifiedCodeCountsTable(CDMdbHandler, domains = domain, stratifiedCodeCountsTable = stratifiedCodeCountsTable)
  )

  stratifiedCodeCounts <- CDMdbHandler$connectionHandler$tbl(I(paste0(resultsDatabaseSchema, ".", stratifiedCodeCountsTable)))

  # check that the table was created 
  stratifiedCodeCounts |>
   dplyr::count() |>
   dplyr::pull(n) |>
   expect_gt(0)
   
  # check that the table was created with correct columns
  stratifiedCodeCounts |>
    head() |>
    dplyr::collect() |>
    colnames() |>
    expect_equal(c(
      "concept_id",
      "maps_to_concept_id",
      "calendar_year",
      "gender_concept_id",
      "age_decile",
      "record_counts"
    ))
})

# test_that("createObservationCountsTable works", {
#   CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
#   withr::defer({
#     CDMdbHandler <- NULL
#     gc()
#   })

#   connection <- CDMdbHandler$connectionHandler$getConnection()
#   cdmDatabaseSchema <- CDMdbHandler$cdmDatabaseSchema
#   resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

#   sqlPath <- system.file("sql", "sql_server", "createObservationCountsTable.sql", package = "ROMOPAPI")
#   sql <- SqlRender::readSql(sqlPath)
#   sql <- SqlRender::render(sql,
#     cdmDatabaseSchema = cdmDatabaseSchema,
#     resultsDatabaseSchema = resultsDatabaseSchema
#   )
#   sql <- SqlRender::translate(sql, targetDialect = connection@dbms)

#   DatabaseConnector::executeSql(connection, sql)

#   observation_counts <- CDMdbHandler$connectionHandler$tbl(paste0(resultsDatabaseSchema, ".observation_counts"))
#   observation_counts |>
#     dplyr::count() |>
#     dplyr::pull(n) |>
#     expect_gt(0)
# })


test_that("createCodeCountsTables works", {
  # only works in a full CDM database
  skip_if(testingDatabase == "OnlyCounts-FinnGen")

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  codeCountsTable <- "code_counts_test0"
  stratifiedCodeCountsTable <- paste0("stratified_", codeCountsTable)
  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0("DROP TABLE ", resultsDatabaseSchema, ".", codeCountsTable))
    CDMdbHandler$connectionHandler$executeSql(paste0("DROP TABLE ", resultsDatabaseSchema, ".", stratifiedCodeCountsTable))
  })

  createCodeCountsTables(CDMdbHandler, codeCountsTable = codeCountsTable)

  # - Check if the table was created
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema
  cdmDatabaseSchema <- CDMdbHandler$cdmDatabaseSchema
  code_counts <- CDMdbHandler$connectionHandler$tbl(I(paste0(resultsDatabaseSchema, ".", codeCountsTable)))

  # check that the table was created with correct columns
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
      "record_counts",
      "descendant_record_counts"
    ))

  # check that descendant_record_counts is greater than or equal to record_counts
  code_counts |>
    dplyr::filter(descendant_record_counts < record_counts) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

  # check that concept_id is unique
  code_counts |>
    dplyr::distinct(concept_id) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(code_counts |> dplyr::count() |> dplyr::pull(n))
})
