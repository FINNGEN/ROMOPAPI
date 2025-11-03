
#' Get logs from ParallelLogger temp file
#'
#' @description
#' Retrieves log messages from the ParallelLogger temporary log file.
#' This may help track internal API events, errors, and debug information.
#'
#' @return A character vector with each log line as a string.
#'
#' @export
getLogs <- function() {
    ParallelLogger::logInfo("getLogs: Getting logs")
    #
    fileName <- getOption("parallelLoggerTempFile")
    logs <- readLines(fileName)
    return(logs)
}
