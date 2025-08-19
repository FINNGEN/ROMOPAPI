#' Create a Plumber API for ROMOPAPI
#'
#' @description
#' Creates a Plumber API router for the ROMOPAPI package with predefined endpoints
#' for health checks and ID processing. The API includes automatic documentation
#' and CORS support.
#'
#' @return A plumber router object with configured endpoints
#'
#' @importFrom plumber pr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Create the API
#'   api <- create_api()
#'   
#'   # Run the API
#'   plumber::pr_run(api, port = 8000)
#'   
#'   # Access health endpoint
#'   # GET /health
#'   
#'   # Process IDs
#'   # GET /process-ids?ids=1,2,3
#' }
#'
#' @section Endpoints:
#' \itemize{
#'   \item `GET /health` - Health status check
#'   \item `GET /process-ids` - Process comma-separated list of IDs
#'   \item `POST /batch-process` - Process batch of IDs via JSON
#' }
create_api <- function() {
  api <- plumber::pr()
  
  #* @apiTitle ROMOPAPI
  #* @apiDescription API to access OMOP CDM database
  
  #* Get health status of the API
  #* @get /health
  function() {
    return(list(status = "healthy", timestamp = Sys.time()))
  }
  
  #* Process a list of IDs
  #* @param ids A comma-separated list of integer IDs
  #* @get /process-ids
  function(ids = NULL) {
    # Parse the comma-separated list into a numeric vector
    if (!is.null(ids)) {
      ids <- as.numeric(strsplit(ids, ",")[[1]])
    }
    
    # Validate that ids is an integer vector
    tryCatch({
      validated_ids <- toAccept(ids, "ids")
      return(list(
        status = "success",
        message = "IDs validated successfully",
        ids = validated_ids
      ))
    }, error = function(e) {
      return(list(
        status = "error",
        message = e$message
      ))
    })
  }
  
  #* Process a batch of IDs
  #* @post /batch-process
  #* @parser json
  function(req) {
    # Extract the IDs from the JSON body
    body <- req$body
    
    if (is.null(body) || is.null(body$ids)) {
      return(list(
        status = "error",
        message = "Request body must contain an 'ids' field"
      ))
    }
    
    # Validate that ids is an integer vector
    tryCatch({
      validated_ids <- toAccept(body$ids, "ids")
      return(list(
        status = "success",
        message = "IDs validated successfully",
        ids = validated_ids
      ))
    }, error = function(e) {
      return(list(
        status = "error",
        message = e$message
      ))
    })
  }
  
  return(api)
} 