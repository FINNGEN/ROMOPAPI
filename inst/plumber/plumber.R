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

#* Get the code counts for a given concept ID
#* @param conceptId The concept ID to get counts and relationships for
#* @get /getCodeCounts
function(res, conceptId=0L) {

  conceptId <- as.integer(conceptId)

  if (is.na(conceptId)) {
    res$status <- 400 # Bad request
    return(list(error = jsonlite::unbox("conceptId must be an integer")))
  }

  tryCatch({
  getCodeCounts_memoise(
    CDMdbHandler = CDMdbHandler,
    conceptId = conceptId
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
  concepts <- getConceptsWithCodeCounts_memoise(CDMdbHandler = CDMdbHandler)
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

  tmp_html <- createReport(conceptId, CDMdbHandler, showsMappings = showsMappings, pruneLevels = pruneLevels, pruneClass = pruneClass)
  # Return the HTML contents
  paste(readLines(tmp_html), collapse = "\n")
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