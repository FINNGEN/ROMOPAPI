test_that("createStratifiedCodeCountsTable works with duplicated counts", {
  # only works in a full CDM database
  skip_if(testingDatabase != "AtlasDevelopment-5k")

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(
    test_cohortTableHandlerConfig,
    loadConnectionChecksLevel = "basicChecks"
  )
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  stratifiedCodeCountsTable <- "stratified_code_counts_test0"
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0(
      "DROP TABLE ",
      resultsDatabaseSchema,
      ".",
      stratifiedCodeCountsTable
    ))
  })

  domain <- tibble::tribble(
    ~domain_id  , ~table_name            , ~concept_id_field      , ~date_field            , ~maps_to_concept_id_field     ,
    "Condition" , "condition_occurrence" , "condition_concept_id" , "condition_start_date" , "condition_source_concept_id"
  )

  # codeAtomicCountsWithDuplicatedCounts
  suppressWarnings(
    createStratifiedCodeCountsTable(
      CDMdbHandler,
      domains = domain,
      stratifiedCodeCountsTable = stratifiedCodeCountsTable
    )
  )

  stratifiedCodeCounts <- CDMdbHandler$connectionHandler$tbl(I(paste0(
    resultsDatabaseSchema,
    ".",
    stratifiedCodeCountsTable
  )))

  # check that the table was created
  nrows <- stratifiedCodeCounts |>
    dplyr::count() |>
    dplyr::pull(n) 
  
  nrows |> expect_gt(0)

  # check that the table was created with correct columns
  stratifiedCodeCounts |>
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
      "record_counts",
      "persons_hll_counts"
    ))
  
  stratifiedCodeCounts |> 
    dplyr::filter(visit_group_concept_id != 0) |> 
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)
})

test_that("createStratifiedCodeCountsTable works with visit_source_group_concept_ids", {
  # only works in a full CDM database
  skip_if(testingDatabase != "AtlasDevelopment-5k")

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(
    test_cohortTableHandlerConfig,
    loadConnectionChecksLevel = "basicChecks"
  )
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  stratifiedCodeCountsTable <- "stratified_code_counts_test0"
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0(
      "DROP TABLE ",
      resultsDatabaseSchema,
      ".",
      stratifiedCodeCountsTable
    ))
  })

  domain <- tibble::tribble(
    ~domain_id  , ~table_name            , ~concept_id_field      , ~date_field            , ~maps_to_concept_id_field     ,
    "Condition" , "condition_occurrence" , "condition_concept_id" , "condition_start_date" , "condition_source_concept_id"
  )

  visitSourceGroupConceptIds = c(
    # longitudinal
    2002330246, # INPAT
    2002330247, # OPER_IN
    2002330248, # OPER_OUT
    2002330249, # OUTPAT
    2002330250, # PRIM_OUT
    2002330102, # REIM
    2002330104, # DEATH
    2002330101, # PURCH
    2002330103, # CANC
    # registers
    2002330245, # KANTA
    2002330106, # BIOBANK
    2002330186, # KIDNEY
    2002330119, # VISION
    2002330105, # BIRTH_MOTHER
    # Drugs
    2002330251, # PRESCRIPTION
    2002330252, # DELIVERY
    2002330253, # PRESCRIPTION_DELIVERY
    2002330254, # DELIVERY_KELA
    2002330255 # PRESCRIPTION_DELIVERY_KELA
  )

  # codeAtomicCountsWithDuplicatedCounts
  suppressWarnings(
    createStratifiedCodeCountsTable(
      CDMdbHandler,
      domains = domain,
      stratifiedCodeCountsTable = stratifiedCodeCountsTable,
      visitSourceGroupConceptIds = visitSourceGroupConceptIds
    )
  )

  stratifiedCodeCounts <- CDMdbHandler$connectionHandler$tbl(I(paste0(
    resultsDatabaseSchema,
    ".",
    stratifiedCodeCountsTable
  )))

  # check that the table was created
  nrows <- stratifiedCodeCounts |>
    dplyr::count() |>
    dplyr::pull(n) 
  
  nrows |> expect_gt(0)

  # check that the table was created with correct columns
  stratifiedCodeCounts |>
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
      "record_counts",
      "persons_hll_counts"
    ))
  
  
  stratifiedCodeCounts |> 
    dplyr::filter(visit_group_concept_id == 0) |> 
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

  ## All the visit_group_concept_id are in the provided visitSourceGroupConceptIds 
  stratifiedCodeCounts |> 
    dplyr::distinct(visit_group_concept_id) |> 
    dplyr::pull(visit_group_concept_id) |>
    (\(x) expect_true(all(x %in% visitSourceGroupConceptIds)))()
})


test_that("createStratifiedCodeCountsTable works with visit_source_group_concept_ids if one missing takes childern", {
  # only works in a full CDM database
  skip_if(testingDatabase != "AtlasDevelopment-5k")

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(
    test_cohortTableHandlerConfig,
    loadConnectionChecksLevel = "basicChecks"
  )
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  stratifiedCodeCountsTable <- "stratified_code_counts_test0"
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0(
      "DROP TABLE ",
      resultsDatabaseSchema,
      ".",
      stratifiedCodeCountsTable
    ))
  })

  domain <- tibble::tribble(
    ~domain_id  , ~table_name            , ~concept_id_field      , ~date_field            , ~maps_to_concept_id_field     ,
    "Condition" , "condition_occurrence" , "condition_concept_id" , "condition_start_date" , "condition_source_concept_id"
  )

  visitSourceGroupConceptIds = c(
    # longitudinal
    2002330246, # INPAT
    2002330247, # OPER_IN
    2002330248, # OPER_OUT
    #2002330249, # OUTPAT
    2002330250, # PRIM_OUT
    2002330102, # REIM
    2002330104, # DEATH
    2002330101, # PURCH
    2002330103, # CANC
    # registers
    2002330245, # KANTA
    2002330106, # BIOBANK
    2002330186, # KIDNEY
    2002330119, # VISION
    2002330105, # BIRTH_MOTHER
    # Drugs
    2002330251, # PRESCRIPTION
    2002330252, # DELIVERY
    2002330253, # PRESCRIPTION_DELIVERY
    2002330254, # DELIVERY_KELA
    2002330255 # PRESCRIPTION_DELIVERY_KELA
  )

  # codeAtomicCountsWithDuplicatedCounts
  suppressWarnings(
    createStratifiedCodeCountsTable(
      CDMdbHandler,
      domains = domain,
      stratifiedCodeCountsTable = stratifiedCodeCountsTable,
      visitSourceGroupConceptIds = visitSourceGroupConceptIds
    )
  )

  stratifiedCodeCounts <- CDMdbHandler$connectionHandler$tbl(I(paste0(
    resultsDatabaseSchema,
    ".",
    stratifiedCodeCountsTable
  )))

  # check that the table was created
  nrows <- stratifiedCodeCounts |>
    dplyr::count() |>
    dplyr::pull(n) 
  
  nrows |> expect_gt(0)

  # check that the table was created with correct columns
  stratifiedCodeCounts |>
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
      "record_counts",
      "persons_hll_counts"
    ))
  
  
  stratifiedCodeCounts |> 
    dplyr::filter(visit_group_concept_id == 0) |> 
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

  ## All the visit_group_concept_id are in the provided visitSourceGroupConceptIds 
  stratifiedCodeCounts |> 
    dplyr::distinct(visit_group_concept_id) |> 
    dplyr::pull(visit_group_concept_id) |>
    (\(x) expect_false(all(x %in% visitSourceGroupConceptIds)))()
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
  skip_if(testingDatabase != "AtlasDevelopment-5k")

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(
    test_cohortTableHandlerConfig,
    loadConnectionChecksLevel = "basicChecks"
  )
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  codeCountsTable <- "code_counts_test0"
  stratifiedCodeCountsTable <- paste0("stratified_", codeCountsTable)
  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0(
      "DROP TABLE ",
      resultsDatabaseSchema,
      ".",
      codeCountsTable
    ))
    CDMdbHandler$connectionHandler$executeSql(paste0(
      "DROP TABLE ",
      resultsDatabaseSchema,
      ".",
      stratifiedCodeCountsTable
    ))
  })

  createCodeCountsTables(CDMdbHandler, codeCountsTable = codeCountsTable)

  # - Check if the table was created
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema
  cdmDatabaseSchema <- CDMdbHandler$cdmDatabaseSchema
  code_counts <- CDMdbHandler$connectionHandler$tbl(I(paste0(
    resultsDatabaseSchema,
    ".",
    codeCountsTable
  )))

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
      "descendant_record_counts",
      "number_of_descendants"
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

  # check that number_of_descendants is greater than or equal to 1
  code_counts |>
    dplyr::filter(number_of_descendants < 1) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

  # check that all with record_counts = descendant_record_counts have  number_of_descendants = 1
  code_counts |>
    dplyr::filter(record_counts == descendant_record_counts) |>
    dplyr::filter(number_of_descendants != 1) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

  # check that all with number_of_descendants > 1 have record_counts > descendant_record_counts
  code_counts |>
    dplyr::filter(number_of_descendants > 1) |>
    dplyr::filter(record_counts > descendant_record_counts) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)
})




test_that("createCodeCountsTables works stratified by visit_group_concept_id", {
  # only works in a full CDM database
  skip_if(testingDatabase != "AtlasDevelopment-5k")

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(
    test_cohortTableHandlerConfig,
    loadConnectionChecksLevel = "basicChecks"
  )
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  codeCountsTable <- "code_counts_test0"
  stratifiedCodeCountsTable <- paste0("stratified_", codeCountsTable)
  withr::defer({
    CDMdbHandler$connectionHandler$executeSql(paste0(
      "DROP TABLE ",
      resultsDatabaseSchema,
      ".",
      codeCountsTable
    ))
    CDMdbHandler$connectionHandler$executeSql(paste0(
      "DROP TABLE ",
      resultsDatabaseSchema,
      ".",
      stratifiedCodeCountsTable
    ))
  })


  visitSourceGroupConceptIds = c(
    # longitudinal
    2002330246, # INPAT
    2002330247, # OPER_IN
    2002330248, # OPER_OUT
    #2002330249, # OUTPAT
    2002330250, # PRIM_OUT
    2002330102, # REIM
    2002330104, # DEATH
    2002330101, # PURCH
    2002330103, # CANC
    # registers
    2002330245, # KANTA
    2002330106, # BIOBANK
    2002330186, # KIDNEY
    2002330119, # VISION
    2002330105, # BIRTH_MOTHER
    # Drugs
    2002330251, # PRESCRIPTION
    2002330252, # DELIVERY
    2002330253, # PRESCRIPTION_DELIVERY
    2002330254, # DELIVERY_KELA
    2002330255 # PRESCRIPTION_DELIVERY_KELA
  )


  createCodeCountsTables(
    CDMdbHandler,
     codeCountsTable = codeCountsTable, 
     visitSourceGroupConceptIds = visitSourceGroupConceptIds
  )

  # - Check if the table was created
  resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema
  cdmDatabaseSchema <- CDMdbHandler$cdmDatabaseSchema
  code_counts <- CDMdbHandler$connectionHandler$tbl(I(paste0(
    resultsDatabaseSchema,
    ".",
    codeCountsTable
  )))

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
      "descendant_record_counts",
      "number_of_descendants"
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

  # check that number_of_descendants is greater than or equal to 1
  code_counts |>
    dplyr::filter(number_of_descendants < 1) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

  # check that all with record_counts = descendant_record_counts have  number_of_descendants = 1
  code_counts |>
    dplyr::filter(record_counts == descendant_record_counts) |>
    dplyr::filter(number_of_descendants != 1) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)

  # check that all with number_of_descendants > 1 have record_counts > descendant_record_counts
  code_counts |>
    dplyr::filter(number_of_descendants > 1) |>
    dplyr::filter(record_counts > descendant_record_counts) |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_equal(0)
})


test_that("stratified table keeps source concepts with no standard concept (concept_id = 0)", {
  # Self-contained regression test: events whose standard concept is unmapped
  # (concept_id = 0) but whose source concept is known (e.g. NOMESCO procedure
  # codes) must NOT be dropped. They should land in the stratified table under
  # maps_to_concept_id, and flow through to code_counts as the source concept.
  # Runs on a throw-away SQLite CDM, so it does not need a full CDM database.

  pathToSqlite <- tempfile(fileext = ".sqlite")
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = pathToSqlite
  )
  connection <- DatabaseConnector::connect(connectionDetails)
  withr::defer({
    DatabaseConnector::disconnect(connection)
    unlink(pathToSqlite)
  })

  cdmDatabaseSchema <- "main"
  resultsDatabaseSchema <- "main"

  # - Minimal OMOP fixtures: one mapped event and one unmapped-source event
  DatabaseConnector::insertTable(
    connection,
    tableName = "person",
    data = tibble::tibble(person_id = 1L, gender_concept_id = 8507L, year_of_birth = 1980L),
    dropTableIfExists = TRUE, createTable = TRUE, tempTable = FALSE
  )
  DatabaseConnector::insertTable(
    connection,
    tableName = "observation_period",
    data = tibble::tibble(
      person_id = 1L,
      observation_period_start_date = "2000-01-01",
      observation_period_end_date = "2030-01-01"
    ),
    dropTableIfExists = TRUE, createTable = TRUE, tempTable = FALSE
  )
  DatabaseConnector::insertTable(
    connection,
    tableName = "condition_occurrence",
    data = tibble::tibble(
      person_id = c(1L, 1L),
      condition_concept_id = c(320128L, 0L), # second row is unmapped
      condition_source_concept_id = c(44831230L, 2000999L), # source known in both
      condition_start_date = c("2010-05-01", "2011-06-01"),
      visit_occurrence_id = c(NA_integer_, NA_integer_)
    ),
    dropTableIfExists = TRUE, createTable = TRUE, tempTable = FALSE
  )
  # concept table needed by createCodeCountsTable (TEMP self-ancestor)
  DatabaseConnector::insertTable(
    connection,
    tableName = "concept",
    data = tibble::tibble(concept_id = c(320128L, 44831230L, 2000999L, 0L)),
    dropTableIfExists = TRUE, createTable = TRUE, tempTable = FALSE
  )
  DatabaseConnector::executeSql(
    connection,
    "CREATE TABLE main.concept_ancestor (
       ancestor_concept_id INTEGER,
       descendant_concept_id INTEGER,
       min_levels_of_separation INTEGER,
       max_levels_of_separation INTEGER
     );",
    progressBar = FALSE, reportOverallTime = FALSE
  )

  domain <- tibble::tribble(
    ~domain_id  , ~table_name            , ~concept_id_field      , ~date_field            , ~maps_to_concept_id_field     ,
    "Condition" , "condition_occurrence" , "condition_concept_id" , "condition_start_date" , "condition_source_concept_id"
  )

  # - Build the stratified table exactly as createStratifiedCodeCountsTable does
  stratifiedCodeCountsTable <- "stratified_code_counts_test_nomesco"
  createSql <- SqlRender::render(
    "DROP TABLE IF EXISTS @resultsDatabaseSchema.@stratifiedCodeCountsTable;
     CREATE TABLE @resultsDatabaseSchema.@stratifiedCodeCountsTable (
       concept_id INTEGER,
       maps_to_concept_id INTEGER,
       visit_group_concept_id INTEGER,
       calendar_year INTEGER,
       gender_concept_id INTEGER,
       age_decile INTEGER,
       record_counts INTEGER,
       persons_hll_counts INTEGER
     )",
    resultsDatabaseSchema = resultsDatabaseSchema,
    stratifiedCodeCountsTable = stratifiedCodeCountsTable
  )
  DatabaseConnector::executeSql(
    connection, SqlRender::translate(createSql, targetDialect = connection@dbms),
    progressBar = FALSE, reportOverallTime = FALSE
  )

  appendSql <- SqlRender::readSql(
    system.file("sql", "sql_server", "appendToStratrifiedCodeCountsTable.sql", package = "ROMOPAPI")
  )
  appendSql <- SqlRender::render(
    appendSql,
    stratifiedCodeCountsTable = stratifiedCodeCountsTable,
    cdmDatabaseSchema = cdmDatabaseSchema,
    resultsDatabaseSchema = resultsDatabaseSchema,
    table_name = domain$table_name,
    concept_id_field = domain$concept_id_field,
    date_field = domain$date_field,
    maps_to_concept_id_field = domain$maps_to_concept_id_field,
    visit_group_concept_ids = "0"
  )
  DatabaseConnector::executeSql(
    connection, SqlRender::translate(appendSql, targetDialect = connection@dbms),
    progressBar = FALSE, reportOverallTime = FALSE
  )

  stratified <- DatabaseConnector::renderTranslateQuerySql(
    connection,
    "SELECT concept_id, maps_to_concept_id, record_counts
       FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable",
    resultsDatabaseSchema = resultsDatabaseSchema,
    stratifiedCodeCountsTable = stratifiedCodeCountsTable
  ) |>
    tibble::as_tibble()
  names(stratified) <- tolower(names(stratified))

  # the unmapped-source event is kept, tagged concept_id = 0 / source id
  stratified |>
    dplyr::filter(concept_id == 0 & maps_to_concept_id == 2000999) |>
    nrow() |>
    expect_equal(1)
  # the mapped event is still there
  stratified |>
    dplyr::filter(concept_id == 320128) |>
    nrow() |>
    expect_equal(1)

  # - Aggregate to code_counts and check the source concept surfaces, no phantom 0
  codeCountsTable <- "code_counts_test_nomesco"
  ccSql <- SqlRender::readSql(
    system.file("sql", "sql_server", "createCodeCountsTable.sql", package = "ROMOPAPI")
  )
  ccSql <- SqlRender::render(
    ccSql,
    cdmDatabaseSchema = cdmDatabaseSchema,
    resultsDatabaseSchema = resultsDatabaseSchema,
    codeCountsTable = codeCountsTable,
    stratifiedCodeCountsTable = stratifiedCodeCountsTable
  )
  DatabaseConnector::executeSql(
    connection, SqlRender::translate(ccSql, targetDialect = connection@dbms),
    progressBar = FALSE, reportOverallTime = FALSE
  )

  codeCounts <- DatabaseConnector::renderTranslateQuerySql(
    connection,
    "SELECT concept_id FROM @resultsDatabaseSchema.@codeCountsTable",
    resultsDatabaseSchema = resultsDatabaseSchema,
    codeCountsTable = codeCountsTable
  ) |>
    tibble::as_tibble()
  names(codeCounts) <- tolower(names(codeCounts))

  # source concept is counted
  expect_true(2000999 %in% codeCounts$concept_id)
  # mapped standard concept is still counted
  expect_true(320128 %in% codeCounts$concept_id)
  # unmapped events are not lumped into a phantom concept_id = 0
  expect_false(0 %in% codeCounts$concept_id)
})
