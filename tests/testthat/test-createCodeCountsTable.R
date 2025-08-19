test_that("appendToCodeCountsTable works", {

  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler$finalize()
  })

  domain <- tibble::tribble(
    ~domain_id, ~table_name, ~concept_id_field, ~date_field,
    "Condition", "condition_occurrence", "condition_source_concept_id", "condition_start_date",
  )
  createCodeCountsTable(CDMdbHandler, domains = domain)
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema
  code_counts <- CDMdbHandler$connectionHandler$tbl(paste0(resultsDatabaseSchema, ".code_counts"))  

  parentNoCouns <- code_counts |> dplyr::filter(concept_id == 40169766)   |> collect()
  childCounts <- code_counts |> dplyr::filter(concept_id == 21115716)   |> collect()

  sideBySide <- parentNoCouns |> dplyr::left_join(
    childCounts,
    by = c("domain", "concept_id", "calendar_year", "gender_concept_id", "age_decile")
  ) 
  
  sideBySide |> dplyr::filter(event_counts.x != 0) |> nrow() |> expect_equal(0)
  sideBySide |> dplyr::filter(person_counts.x != 0) |> nrow() |> expect_equal(0)
  sideBySide |> dplyr::filter(incidence_person_counts.x != 0) |> nrow() |> expect_equal(0)

  sideBySide |> dplyr::filter(event_counts.y == 0) |> nrow() |> expect_equal(0)
  sideBySide |> dplyr::filter(person_counts.y == 0) |> nrow() |> expect_equal(0)
  sideBySide |> dplyr::filter(incidence_person_counts.y == 0) |> nrow() |> expect_equal(0)

  sideBySide |> dplyr::filter(descendant_event_counts.x != event_counts.y) |> nrow() |> expect_equal(0)
  sideBySide |> dplyr::filter(descendant_person_counts.x != person_counts.y) |> nrow() |> expect_equal(0)
  sideBySide |> dplyr::filter(descendant_incidence_person_counts.x != incidence_person_counts.y) |> nrow() |> expect_equal(0)

})

test_that("createCodeCountsTable works", {
  CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler$finalize()
  })

  createCodeCountsTable(CDMdbHandler)

  # - Check if the table was created
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema
  code_counts <- CDMdbHandler$connectionHandler$tbl(paste0(resultsDatabaseSchema, ".code_counts"))

  code_counts |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)
  code_counts |>
    colnames() |>
    expect_equal(c(
      "domain", "concept_id",
      "calendar_year", "gender_concept_id", "age_decile",
      "event_counts", "person_counts", "incidence_person_counts",
      "descendant_event_counts", "descendant_person_counts", "descendant_incidence_person_counts",
      "total_person_counts"
    ))
  code_counts |>
    dplyr::filter(person_counts > event_counts) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)
  code_counts |>
    dplyr::filter(incidence_person_counts > person_counts) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)
  code_counts |>
    dplyr::filter(descendant_person_counts < person_counts) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)
  code_counts |>
    dplyr::filter(descendant_incidence_person_counts < incidence_person_counts) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)
  code_counts |>
    dplyr::filter(descendant_event_counts < event_counts) |> collect() |> 
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)


  if (testingDatabase == "Eunomia-FinnGen") {
    code_counts |>
      dplyr::count() |>
      dplyr::pull(n) |>
      expect_equal(303511)
  }

  # check that all the conditions in code_counts are in the condition_occurrence table
  condition_occurrence <- CDMdbHandler$connectionHandler$tbl(paste0(cdmDatabaseSchema, ".condition_occurrence"))
  observation_period <- CDMdbHandler$connectionHandler$tbl(paste0(cdmDatabaseSchema, ".observation_period"))
  conditionsInObservationPeriod <- condition_occurrence |>
    dplyr::inner_join(observation_period, by = c("person_id" = "person_id")) |>
    dplyr::filter(observation_period_start_date <= condition_start_date) |>
    dplyr::filter(observation_period_end_date >= condition_end_date)
  conditionConceptIds <- conditionsInObservationPeriod |>
    dplyr::filter(condition_concept_id != 0) |>
    dplyr::distinct(condition_concept_id) |>
    dplyr::pull(condition_concept_id)
  conditionSourceConceptIds <- conditionsInObservationPeriod |>
    dplyr::filter(condition_source_concept_id != 0) |>
    dplyr::distinct(condition_source_concept_id) |>
    dplyr::pull(condition_source_concept_id)
  allConditionConceptIds <- c(conditionSourceConceptIds, conditionConceptIds) |>
    unique() |>
    sort()

  conditionCountsIds <- code_counts |>
    dplyr::filter(domain == "Condition") |>
    dplyr::distinct(concept_id) |>
    dplyr::pull(concept_id) |>
    sort()
  expect_equal(allConditionConceptIds, conditionCountsIds)
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