#!/usr/bin/env Rscript

# Load the ROMOPAPI package
library(ROMOPAPI)

# Create the API
api <- create_api()

# Run the API on port 8000
message("Starting ROMOPAPI on port 8000...")
plumber::pr_run(api, port = 8000) 