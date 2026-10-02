# ROMOPAPI 2.4.0
- /getCodeCounts is split into /getConceptRelationships (concept tree and concept details) and /getCodeCountsStratified (per-stratum record counts)
- New /getPersonCountsUpset and /getPersonCountsFilters endpoints, returning exact person counts for a list of tagged concept sets (`<conceptId><S|M><D?>`), with `yearsRange`, `sexStratum`, `ageStratum` and `visitStratum` filters
- New `stratified_persons` table with person level counts, and new `number_of_descendants`, `person_counts` and `descendant_person_counts` columns in `code_counts`
- Added BigQuery specific SQL, the dialect is now chosen from the connection dbms
- New `visitSourceGroupConceptIds` parameter in `runApiServer()`, also read from the database config
- /report includes a person counts section with upset, sex, age and visit plots
- Fixed stratified code counts dropping events with an unmapped standard concept (e.g. NOMESCO procedure codes)
- Docker image is built from the local source and published on merge to development

# ROMOPAPI 2.3.0
- /getCodeCounts now retuns one more column `visit_group_concept_id` in `stratified_code_counts` table, this is the register they come from 
- New endpoint /getVisitTypeNames returns a table with the names and codes for `visit_group_concept_id`s

# ROMOPAPI 2.2.0
- Added number_of_descendants to concepts with code counts
- Added conceptId 21600744 to testing data

# ROMOPAPI 2.1.1
- Added caching concepts with code counts at startup

# ROMOPAPI 2.1.0
- Added getAPIInfo endpoint to API
- Updated db with large tree
- Fixed error in getCodeCounts when conceptId was the root of the tree

# ROMOPAPI 2.0.0
- Updated to use DatabaseConnector v7
  
# ROMOPAPI 1.0.0

Initital stable release.
