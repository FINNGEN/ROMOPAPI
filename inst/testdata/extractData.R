
#
# DEPENDENCIES
#
source("data/fct_extractDataFromOMOP/extractDataFromOMOP.R")

#
# PARAMETERS
#
  bigrquery::bq_auth(path = Sys.getenv("GCP_SERVICE_KEY"))
options(sqlRenderTempEmulationSchema = 'atlas-development-270609.sandbox')
connectionDetails <- DatabaseConnector::createDbiConnectionDetails(
  dbms = "bigquery",
  drv = bigrquery::bigquery(),
  project = "atlas-development-270609",
  billing = "atlas-development-270609",
  bigint = "integer64"
)

connectionDetails  <- connectionDetails
vocabularyDatabaseSchema <- "atlas-development-270609.etl_sam_dev_omop"
cdmDatabaseSchema <- "atlas-development-270609.etl_sam_dev_omop"
scratchDatabaseSchema <- "atlas-development-270609.sandbox"
exportFolder <- "data/testData/AtlasDev"

#
# EXECUTE
#
fct_extractDataFromOMOP(
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    scratchDatabaseSchema = scratchDatabaseSchema,
    exportFolder = exportFolder
)

