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
function(
    res, conceptId) {
  if (!grepl("^\\d+$", conceptId)) {
    res$status <- 400 # Bad request
    return(list(error = jsonlite::unbox("conceptId must be an integer")))
  }

  conceptId <- as.integer(conceptId)

  getCodeCounts_memoise(
    CDMdbHandler = CDMdbHandler,
    conceptId = conceptId
  )
}


#* Get the CDM source information
#* @get /getCDMSource
function() {
  getCDMSource(
    CDMdbHandler = CDMdbHandler
  )
}

#* Get the list of concepts with code counts
#* @get /getListOfConcepts
function() {
 getListOfConcepts_memoise(CDMdbHandler = CDMdbHandler)
}


#* Serve mermaid.min.js directly
#* @get /mermaid.min.js
#* @serializer contentType list(type = "application/javascript")
function() {
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

#* @get /report
#* @param conceptId The concept ID to include in the report
#* @serializer html
function(conceptId = NULL, showsMappings = FALSE) {
  conceptId <- as.integer(conceptId)
  showsMappings <- as.logical(showsMappings)
  # Render Rmd to a temporary HTML file
  tmp_html <- tempfile(fileext = ".html")
  rmarkdown::render(system.file("reports", "testReport.Rmd", package = "ROMOPAPI"), 
                    output_file = tmp_html, 
                    quiet = TRUE,
                    params = list(conceptId = conceptId, CDMdbHandler = CDMdbHandler, showsMappings = showsMappings))
  
  # Return the HTML contents
  paste(readLines(tmp_html), collapse = "\n")
}
