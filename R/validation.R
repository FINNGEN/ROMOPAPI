#' Check if a value is an integer vector
#'
#' This function validates that the provided value is an integer vector.
#' It will throw an error if the value is not an integer vector.
#'
#' @param value The value to check
#' @param parameterName The name of the parameter (for error messages)
#' @param allowNA Should NA values be allowed? Default is FALSE
#' @param allowNull Should NULL values be allowed? Default is FALSE
#'
#' @return The original value if it passes validation
#' @export
#'
#' @examples
#' # Valid integer vector
#' toAccept(c(1L, 2L, 3L), "myParam")
#'
#' # Will throw an error
#' \dontrun{
#'   toAccept(c(1.5, 2.5), "myParam")
#'   toAccept("not a number", "myParam")
#' }
toAccept <- function(value, parameterName, allowNA = FALSE, allowNull = FALSE) {
  # Check for NULL
  if (is.null(value)) {
    if (allowNull) {
      return(value)
    } else {
      stop(paste0("Parameter '", parameterName, "' cannot be NULL"))
    }
  }
  
  # Check if it's a vector
  if (!is.vector(value)) {
    stop(paste0("Parameter '", parameterName, "' must be a vector, got ", class(value)))
  }
  
  # Check if it contains NA values
  if (!allowNA && any(is.na(value))) {
    stop(paste0("Parameter '", parameterName, "' cannot contain NA values"))
  }
  
  # Check if all elements are integers
  if (!all(is.integer(value) | (is.numeric(value) & value == as.integer(value)))) {
    stop(paste0("Parameter '", parameterName, "' must contain only integer values"))
  }
  
  # If all checks pass, return the value
  # Convert numeric integers to actual integers
  if (is.numeric(value) && !is.integer(value)) {
    value <- as.integer(value)
  }
  
  return(value)
} 