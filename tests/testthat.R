# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(ROMOPAPI)


# Post-counts functions against the shipped sqlite counts fixture
Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "OnlyCounts-FinnGen")
Sys.setenv(BUILD_COUNTS_TABLE = "FALSE")
test_check("ROMOPAPI")

# Counts-table creation from a raw OMOP CDM (sqlite)
Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "Eunomia-GiBleed")
Sys.setenv(BUILD_COUNTS_TABLE = "TRUE")
test_check("ROMOPAPI")

# BigQuery: BUILD_COUNTS_TABLE builds the code_counts table that post-counts
# tests read; counts-creation tests build/drop their own throwaway tables.
Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "AtlasDevelopment-5k")
Sys.setenv(BUILD_COUNTS_TABLE = "TRUE")
test_check("ROMOPAPI")