# ROMOPAPI

API to access OMOP CDM (Observational Medical Outcomes Partnership Common Data Model) database

## Installation

You can install ROMOPAPI from GitHub with:

```r
install.packages("remotes")
remotes::install_github("FINNGEN/ROMOPAPI")
```

## Usage

### Running the API Server with the default testing database

The simplest way to start the API server is:

Create a folder to store the testing databases. This saves time as the databases dont have to be downloaded every time the API server is started.

```r
library(ROMOPAPI)

# Run the API server with default settings (uses internal testing database with minimal data)
runApiServer()
```

This will start the API server on port 8585.

#### Testing endpoints

The main endpoint for testing is: http://127.0.0.1:8585/report?conceptId=<concept_id>

For example, http://127.0.0.1:8585/report?conceptId=317009

This will return an HTML report for the concept id 317009.

#### Production endpoints

The main endpoint for production is: http://127.0.0.1:8585/getCodeCounts?conceptId=<concept_id>

This will return the code counts for the concept ids 317009.

Separated in 3 tables:

- `concept_relationship`: relationships between conceptIds
- `concept`: information about the conceptIds
- `stratified_code_counts`: patient counts by conceptId stratified by gender, year, and age decile


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

If it is the first time running the API server with the custom database, you need add the parameter `buildCountsTable = TRUE` to the `runApiServer` function.

```r
databaseConfig <- yaml::read_yaml("path/to/database_config.yml")

ROMOPAPI::runApiServer(
  cohortTableHandlerConfig = databaseConfig$cohortTableHandler,
  buildCountsTable = TRUE
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
   - parent_concept_id: 🔑 Parent concept ID
   - child_concept_id: 🔑 Child concept ID 
   - levels: Hierarchy level information (e.g. "-1", "0", "1-1", "2-2", "3-3", "Mapped from")
    - "-1" means the child is a parent of the root concept
    - "0" means the concept is the root of the hierarchy
    - "n-n" means the child is n levels below the parent in the hierarchy
    - "Mapped from" means the child is mapped from another vocabulary
    - "Maps to" means the child is mapped to another vocabulary
   - concept_class_id: Type of concept (e.g. "Clinical Finding", "ICD10 code")

2. stratified_code_counts - A tibble containing:
   - concept_id: 🔑Concept identifier
   - calendar_year: Year of the counts
   - gender_concept_id: Gender concept ID
   - age_decile: Age group by decade
   - node_record_counts: Number of events for this specific concept
   - node_descendant_record_counts: Number of events including descendant concepts

3. concepts - A tibble containing concept :
   - concept_id: 🔑 Concept identifier
   - concept_name: Name/description of the concept
   - domain_id: Domain the concept belongs to (e.g. "Condition")
   - vocabulary_id: Source vocabulary (e.g. "SNOMED", "ICD10")
   - concept_class_id: Type of concept (e.g. "Clinical Finding", "ICD10 code")
   - standard_concept: Boolean indicating if this is a standard concept
   - concept_code: Original code in source vocabulary
   - record_counts: Number of events for this specific concept
   - descendant_record_counts: Number of events including descendant concepts

# Docker

## Existing docker image

Existing docker image is available at: https://hub.docker.com/repository/docker/javiergrata/romopapi/

```
docker run -p 8585:8585 javiergrata/romopapi
```

## Build the docker image

A GITHUBPAT.txt file with the GitHub personal access token is needed in the root of the repository.

```
docker build --secret id=build_github_pat,src=GITHUBPAT.txt -t romopapi .
```

Optionally, the ROMOPAPI_BRANCH can be set to the branch to be used the BUILD_CACHE_BUSTER can be set current time to force a rebuild of changes in the repository.

```
docker build --secret id=build_github_pat,src=GITHUBPAT.txt --build-arg ROMOPAPI_BRANCH=main --build-arg BUILD_CACHE_BUSTER=$(date +%s) -t romopapi .
```

## Run the docker container

```
docker run -p 8585:8585 romopapi
```


