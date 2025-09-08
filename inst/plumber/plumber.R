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
#* @param conceptIds A vector of concept IDs
#* @get /getCodeCounts
function(
    res, conceptIds) {
  if (!grepl("^\\d+(,\\d+)*$", conceptIds)) {
    res$status <- 400 # Bad request
    return(list(error = jsonlite::unbox("conceptIds must be a comma-separated list of integers")))
  }

  conceptIds <- as.integer(strsplit(conceptIds, ",")[[1]])

  getCodeCounts_memoise(
    CDMdbHandler = CDMdbHandler,
    conceptIds = conceptIds
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



#* @get /report
#* @param conceptId The concept ID to include in the report
#* @serializer html
function(conceptId = NULL) {
  # Render Rmd to a temporary HTML file
  tmp_html <- tempfile(fileext = ".html")
  rmarkdown::render(system.file("reports", "testReport.Rmd", package = "ROMOPAPI"), 
                    output_file = tmp_html, 
                    quiet = TRUE,
                    params = list(conceptId = conceptId))
  
  # Return the HTML contents
  paste(readLines(tmp_html), collapse = "\n")
}
