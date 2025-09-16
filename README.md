# ROMOPAPI

API to access OMOP CDM (Observational Medical Outcomes Partnership Common Data Model) database

## Installation

You can install ROMOPAPI from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("FINNGEN/ROMOPAPI")
```

## Usage

### Running the API Server with the default testing database

The simplest way to start the API server is:

Create a folder to store the testing databases. This saves time as the databases dont have to be downloaded every time the API server is started.

```r
library(ROMOPAPI)


# Set the folder to store the testing databases
Sys.setenv(EUNOMIA_DATA_FOLDER = "<full_path_to_local_eunomia_data_folder>")

# Run the API server with default settings (uses test FinnGen Eunomia database)
runApiServer()

```

This will start the API server on port 8585.

The main endpoint is: http://127.0.0.1:8585/getCodeCounts?conceptIds=<concept_id_1>,<concept_id_2>,...

For example, http://127.0.0.1:8585/getCodeCounts?conceptIds=201826

This will return the code counts for the concept ids 201826.

Separated in 3 tables:

- `concept_relationship`: relationships between conceptIds
- `concept`: information about the conceptIds
- `code_counts`: patient counts by conceptId stratified by gender, year, and age decile



See the API documentation for more details: http://127.0.0.1:8585/__docs__/


### Running the API Server with a custom database

Create a custom database configuration.

Create a yaml file with the database configuration.

For example, `database_config.yml`:
```yaml     
cohortTableHandler:
    database:
      databaseId: F1
      databaseName: FinnGen
      databaseDescription: Eunomia database FinnGen
    connection:
      connectionDetailsSettings:
          dbms: sqlite
          server: <pathToFinnGenEunomiaSqlite>
    cdm:
        cdmDatabaseSchema: main
        vocabularyDatabaseSchema: main
        resultsDatabaseSchema: main
```

If it is the first time running the API server with the custom database, you need to run the following command to create the code counts table.

```r
databaseConfig <- yaml::read_yaml("path/to/database_config.yml")

 CDMdbHandler  <- HadesExtras_createCDMdbHandlerFromList(
  databaseConfig$cohortTableHandler, 
  loadConnectionChecksLevel = "basicChecks"
)
 
ROMOPAPI::createCodeCountsTable(
  CDMdbHandler = CDMdbHandler
)
```

Run the API server with the custom configuration.

```r
library(ROMOPAPI)

databaseConfig <- yaml::read_yaml("path/to/database_config.yml")

# Run the API server with custom configuration
runApiServer(
  cohortTableHandlerConfig = databaseConfig,
  host = "0.0.0.0",  # Allow external connections
  port = 8080
)
```


# DEVELOPMENT

## Run API with AtlasDevelopment

```r
library(ROMOPAPI)

databasesConfig <- HadesExtras_readAndParseYaml(system.file("testdata", "config", "atlasDev_databasesConfig.yml", package = "ROMOPAPI"))
bigrquery::bq_auth(path = Sys.getenv("GCP_SERVICE_KEY"))
# Run the API server with default settings (uses test FinnGen Eunomia database)
runApiServer(cohortTableHandlerConfig = databasesConfig$BQ5k$cohortTableHandler)

```


# API return values

## getCodeCounts


Based on the example output, getCodeCounts returns a list with 3 components:

1. concept_relationships - A tibble containing:
   - concept_id_1: 🔑 Source concept ID
   - concept_id_2: 🔑 Target concept ID 
   - relationship_id:  Type of relationship (e.g. "Parent", "Root", "Mapped from", etc.)
    - "Parent" means that concept_id_1 is a parent of concept_id_2
    - "Root" concept_id is same as concept_id_2 and is the root of the concept hierarchy
    - "Mapped from" means that concept_id_1 is mapped from concept_id_2
    - "Mapped to" means that concept_id_1 is mapped to concept_id_2
    - "n-m" means that concept_id_1 is a descendant of concept_id_2, where n is the min number of levels of the hierarchy and m is the max number of levels of the hierarchy

2. code_counts - A tibble containing:
   - concept_id: 🔑Concept identifier
   - calendar_year: Year of the counts
   - gender_concept_id: Gender concept ID
   - age_decile: Age group by decade
   - record_counts: Number of events for this specific concept
   - descendant_record_counts: Number of events including descendant concepts

3. concepts - A tibble containing concept :
 as seen in [concept table](https://ohdsi.github.io/CommonDataModel/cdm54.html#concept)
 where concept_id is the key column.
