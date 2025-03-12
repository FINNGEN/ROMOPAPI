# ROMOPAPI

API to access OMOP CDM database

## Installation

You can install the development version of ROMOPAPI from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("javier-gracia-tabuenca-tuni/ROMOPAPI")
```

## Usage

### Validation Functions

ROMOPAPI provides validation functions to ensure your data meets the expected format:

```r
library(ROMOPAPI)

# Validate integer vectors
ids <- c(1, 2, 3)
validated_ids <- toAccept(ids, "ids")
print(validated_ids)  # Will be converted to integer: 1L, 2L, 3L

# This will throw an error
tryCatch({
  toAccept(c(1.5, 2.5), "ids")
}, error = function(e) {
  print(e$message)
})
```

### REST API

ROMOPAPI includes a plumber API that you can use to expose your functions as REST endpoints:

```r
library(ROMOPAPI)

# Create the API
api <- create_api()

# Run the API
plumber::pr_run(api, port = 8000)
```

Once the API is running, you can access the following endpoints:

- `GET /health`: Check the health status of the API
- `GET /process-ids?ids=1,2,3`: Process a comma-separated list of integer IDs
- `POST /batch-process`: Process a batch of IDs (JSON body with an "ids" field)

Example of using the API with curl:

```bash
# Check health
curl http://localhost:8000/health

# Process IDs via GET
curl http://localhost:8000/process-ids?ids=1,2,3

# Process IDs via POST
curl -X POST http://localhost:8000/batch-process \
  -H "Content-Type: application/json" \
  -d '{"ids": [1, 2, 3]}'
```

## License

MIT

