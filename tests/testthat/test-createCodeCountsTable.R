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
      "record_counts"
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
      "record_counts"
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
      "record_counts"
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
