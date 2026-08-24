# plumber.R

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg = "") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Get the concept relationships and concept details for a given concept ID
#* @param conceptId The concept ID to get relationships and details for
#* @get /getConceptRelationships
function(res, conceptId=0L) {

  conceptId <- as.integer(conceptId)

  if (is.na(conceptId)) {
    res$status <- 400 # Bad request
    return(list(error = jsonlite::unbox("conceptId must be an integer")))
  }

  tryCatch({
  getConceptRelationships_memoise(
    CDMdbHandler = CDMdbHandler,
    conceptId = conceptId
  )
  }, error = function(e) {
    res$status <- 400
    return(list(error = jsonlite::unbox(e$message)))
  })
}

#* Get the stratified code counts for a given concept ID
#* @param conceptId The concept ID to get stratified counts for
#* @get /getCodeCountsStratified
function(res, conceptId=0L) {

  conceptId <- as.integer(conceptId)

  if (is.na(conceptId)) {
    res$status <- 400 # Bad request
    return(list(error = jsonlite::unbox("conceptId must be an integer")))
  }

  tryCatch({
  getCodeCountsStratified_memoise(
    CDMdbHandler = CDMdbHandler,
    conceptId = conceptId
  )
  }, error = function(e) {
    res$status <- 400
    return(list(error = jsonlite::unbox(e$message)))
  })
}

# Splits "a,b" into an integer vector; "" -> NULL. Shared by the person-counts endpoints below.
.plumberParseIntCsv <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) {
    return(NULL)
  }
  as.integer(strsplit(x, ",")[[1]])
}

# Splits "startYear,endYear" into a length-2 integer vector; "" -> NULL. Errors (via NA) on
# any other length so the caller's is.na()-based 400 check catches malformed input.
.plumberParseYearsRange <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) {
    return(NULL)
  }
  parts <- as.integer(strsplit(x, ",")[[1]])
  if (length(parts) != 2) {
    return(NA_integer_)
  }
  parts
}

#* Get person-count breakdowns by sex, age, visit type and year for a list of concept sets
#* @param conceptIds Comma-separated tagged concept ids, e.g. "317009SD,2000403993MD" —
#*   each token is <conceptId><S|M><D?>: S/M picks concept_id vs maps_to_concept_id,
#*   trailing D expands to the concept and all its descendants
#* @param yearsRange Comma-separated "startYear,endYear" marking the selected year strata.
#*   Omit to select none
#* @param sexStratum Comma-separated gender_concept_id values marking the selected sex strata
#* @param ageStratum Comma-separated age_decile values marking the selected age strata
#* @param visitStratum Comma-separated visit_group_concept_id values marking the selected visit strata
#* @get /getPersonCountsFilters
function(res, conceptIds = "", yearsRange = "", sexStratum = "", ageStratum = "", visitStratum = "") {

  if (!nzchar(trimws(conceptIds))) {
    res$status <- 400
    return(list(error = jsonlite::unbox("conceptIds must not be empty")))
  }

  yearsRange <- .plumberParseYearsRange(yearsRange)
  if (length(yearsRange) == 1 && is.na(yearsRange)) {
    res$status <- 400
    return(list(error = jsonlite::unbox("yearsRange must be \"startYear,endYear\"")))
  }

  tryCatch({
    getPersonCountsFilters_memoise(
      CDMdbHandler = CDMdbHandler,
      conceptIds = conceptIds,
      yearsRange = yearsRange,
      sexStratum = .plumberParseIntCsv(sexStratum),
      ageStratum = .plumberParseIntCsv(ageStratum),
      visitStratum = .plumberParseIntCsv(visitStratum)
    )
  }, error = function(e) {
    res$status <- 400
    return(list(error = jsonlite::unbox(e$message)))
  })
}

#* Get exact set-overlap (UpSet) person counts for a list of concept sets
#* @param conceptIds Comma-separated tagged concept ids, e.g. "317009SD,2000403993MD" —
#*   each token is <conceptId><S|M><D?>: S/M picks concept_id vs maps_to_concept_id,
#*   trailing D expands to the concept and all its descendants. Each token is its own
#*   set in the returned regions
#* @param yearsRange Comma-separated "startYear,endYear" to restrict to. Omit for the full range
#* @param sexStratum Comma-separated gender_concept_id values to restrict to
#* @param ageStratum Comma-separated age_decile values to restrict to
#* @param visitStratum Comma-separated visit_group_concept_id values to restrict to
#* @get /getPersonCountsUpset
function(res, conceptIds = "", yearsRange = "", sexStratum = "", ageStratum = "", visitStratum = "") {

  if (!nzchar(trimws(conceptIds))) {
    res$status <- 400
    return(list(error = jsonlite::unbox("conceptIds must not be empty")))
  }

  yearsRange <- .plumberParseYearsRange(yearsRange)
  if (length(yearsRange) == 1 && is.na(yearsRange)) {
    res$status <- 400
    return(list(error = jsonlite::unbox("yearsRange must be \"startYear,endYear\"")))
  }

  tryCatch({
    getPersonCountsUpset_memoise(
      CDMdbHandler = CDMdbHandler,
      conceptIds = conceptIds,
      yearsRange = yearsRange,
      sexStratum = .plumberParseIntCsv(sexStratum),
      ageStratum = .plumberParseIntCsv(ageStratum),
      visitStratum = .plumberParseIntCsv(visitStratum)
    )
  }, error = function(e) {
    res$status <- 400
    return(list(error = jsonlite::unbox(e$message)))
  })
}

#* Get the API information
#* @get /getAPIInfo
function() {
  getAPIInfo(
    CDMdbHandler = CDMdbHandler
  )
}

#* Get the list of concepts with code counts
#* @get /getListOfConcepts
function() {
  concepts <- getAllConceptsInfo_memoise(CDMdbHandler = CDMdbHandler)
  concepts <- concepts |>
    dplyr::select(concept_id, concept_name, vocabulary_id, concept_code)
  return(concepts)
}

#* @get /report
#* @param conceptId The concept ID to include in the report
#* @serializer html
function(res, conceptId=0L, showsMappings = FALSE, pruneLevels = 0L, pruneClass = '') {

  conceptId <- as.integer(conceptId)
  showsMappings <- as.logical(showsMappings)
  pruneLevels <- as.integer(pruneLevels)
  pruneClass <- as.character(pruneClass)

  if (is.na(conceptId)) {
    res$status <- 400
    return(list(error = jsonlite::unbox("conceptId must be an integer")))
  }
  if (is.na(showsMappings)) {
    res$status <- 400
    return(list(error = jsonlite::unbox("showsMappings must be a logical")))
  }
  if (is.na(pruneLevels)) {
    res$status <- 400
    return(list(error = jsonlite::unbox("pruneLevels must be an integer")))
  }
  if (is.na(pruneClass)) {
    res$status <- 400
    return(list(error = jsonlite::unbox("pruneClass must be a character")))
  }

  tryCatch({
    tmp_html <- createReport(conceptId, CDMdbHandler, showsMappings = showsMappings, pruneLevels = pruneLevels, pruneClass = pruneClass)
    # Return the HTML contents
    paste(readLines(tmp_html), collapse = "\n")
  }, error = function(e) {
    # @serializer html expects a character value, not a list — a plain string here
    res$status <- 400
    paste0("<p>Error: ", e$message, "</p>")
  })
}

#* Serve mermaid.min.js directly
#* @get /mermaid.min.js
#* @serializer contentType list(type = "application/javascript")
function(res) {
  # Get the path to the mermaid.min.js file
  file_path <- system.file("reports", "mermaid.min.js", package = "ROMOPAPI")

  # Check if file exists
  if (!file.exists(file_path)) {
    res$status <- 404
    return(list(error = "File not found"))
  }

  # Read and return the file content
  readChar(file_path, file.info(file_path)$size)
}

#* Get the logs
#* @get /getLogs
function() {
  logs <- getLogs()
  return(logs)
}

#* Send feedback to the API server
#* @post /sendFeedback
function(res, feedback = "") {
  feedback <- as.character(feedback)
  sendFeedback(feedback)
  res$status <- 200
  return(list(message = "Feedback sent"))
}

#* Get the list of visit type names
#* @get /getVisitTypeNames
function() {
  getVisitTypeNames_memoise(CDMdbHandler = CDMdbHandler)
}