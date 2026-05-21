# Get connection
Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-full")
Sys.setenv(BUILD_COUNTS_TABLE = "FALSE")
source("tests/testthat/setup.R")

# List of conceptIds to extract
conceptIds <- c(
    317009, # Snomed: Asthma
    45596282, # ICD10: Asthma
    21601855, # ATC level 4: C10AA (Statins)
    320136, # Big graph, parent of Asthma snomed concept (Disorders of the respiratory system)
    4024567, # biger
    21600744 # bug in plot
)

CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(
    test_cohortTableHandlerConfig,
    loadConnectionChecksLevel = "basicChecks"
)
# uncomment to create code counts tables

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

createCodeCountsTables(
    CDMdbHandler,
    visitSourceGroupConceptIds = visitSourceGroupConceptIds
)
helper_createSqliteDatabaseFromDatabase(
    CDMdbHandler,
    conceptIds = conceptIds,
    pathToSqliteDatabase = "inst/testdata/data/FinnGenR13_countsOnly.sqlite"
)


# Test
connection <- DatabaseConnector::connect(DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = "inst/testdata/data/FinnGenR13_countsOnly.sqlite"
))

DatabaseConnector::dbListTables(connection) |>
    sort() |>
    expect_equal(c(
        "cdm_source",
        "code_counts",
        "concept",
        "concept_ancestor",
        "stratified_code_counts"
    ))

dplyr::tbl(connection, "concept") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

dplyr::tbl(connection, "concept_ancestor") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

dplyr::tbl(connection, "code_counts") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

dplyr::tbl(connection, "stratified_code_counts") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

dplyr::tbl(connection, "cdm_source") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_gt(0)

# visit_group_concept_id
dplyr::tbl(connection, "stratified_code_counts") |>
    count(visit_group_concept_id) |> 
    left_join(dplyr::tbl(connection, "concept"), by=c("visit_group_concept_id"="concept_id")) |> 
    print(n =2122)

dplyr::tbl(connection, "stratified_code_counts") |>
    dplyr::distinct(visit_group_concept_id) |> 
    dplyr::pull(visit_group_concept_id) |>
    (\(x) expect_false(all(x %in% visitSourceGroupConceptIds)))()
