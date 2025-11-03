

databaseConfig <- yaml::read_yaml("inst/testdata/config/onlyCounts_databasesConfig.yml")

ROMOPAPI::runApiServer(
  cohortTableHandlerConfig = databaseConfig$cohortTableHandler,
  buildCountsTable = FALSE
)

# http://127.0.0.1:8585/__docs__/
