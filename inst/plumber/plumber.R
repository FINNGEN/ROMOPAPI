# plumber.R

#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Get the CDM source information
#* @get /getCDMSource
function(){
  getCDMSource(
    CDMdbHandler = CDMdbHandler
  )
}


#* Get the code counts for a given concept ID
#* @param conceptIds A vector of concept IDs
#* @get /getCodeCounts
function(
  res, conceptIds
){

  if (!grepl("^\\d+(,\\d+)*$", conceptIds)) {
    res$status <- 400 # Bad request
    return(list(error=jsonlite::unbox("conceptIds must be a comma-separated list of integers")))
  }

  conceptIds <- as.integer(strsplit(conceptIds, ",")[[1]])
  
  getCodeCounts(
    CDMdbHandler = CDMdbHandler, 
    conceptIds = conceptIds
  )
}


