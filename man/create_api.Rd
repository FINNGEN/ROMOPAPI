% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{create_api}
\alias{create_api}
\title{Create a Plumber API for ROMOPAPI}
\usage{
create_api()
}
\value{
A plumber router object
}
\description{
This function creates a plumber API for the ROMOPAPI package.
It sets up endpoints that use the validation functions.
}
\examples{
\dontrun{
  # Create the API
  api <- create_api()
  
  # Run the API
  plumber::pr_run(api, port = 8000)
}
}
