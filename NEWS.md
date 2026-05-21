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
