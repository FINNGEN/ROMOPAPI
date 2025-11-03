
#' Set Up Logger
#'
#' Sets up a logger with console and file appenders for logging.
#'
#' @param sToken The session token to be added to log entries
#'
#' @return A logger object.
#'
#' @export
fct_setUpLogger  <- function(sToken=""){

  timestamp  <- timestamp <- as.character(as.numeric(format(Sys.time(), "%d%m%Y%H%M%OS2"))*100)
  folderWithLog <- file.path(tempdir(),  paste0("logs", timestamp))
  dir.create(folderWithLog, showWarnings = FALSE)
  fileName <- file.path(folderWithLog, "log.txt")
  options(parallelLoggerTempFile = fileName)
  
  logger <- ParallelLogger::createLogger(
    threshold = "TRACE",
    appenders = list(
      # to console for tracking
      .createConsoleAppenderForSandboxLogging(sToken=sToken),
      # to file for showing in app
      ParallelLogger::createFileAppender(
        fileName = fileName,
        layout = ParallelLogger::layoutTimestamp
      )
    )
  )
  ParallelLogger::clearLoggers()
  #addDefaultFileLogger(file.path(folderWithLog, "log2.txt"))
  ParallelLogger::registerLogger(logger)

}


#' Create Console Appender for Sandbox Logging
#'
#' Creates a console appender for sandbox logging with a specified layout.
#'
#' @param layout A layout function for the logger. Defaults to ParallelLogger::layoutParallel.
#' @param sToken The session token to be added to log entries
#'
#' @return An appender object for logging.
#'
.createConsoleAppenderForSandboxLogging <- function(layout = ParallelLogger::layoutParallel,sToken) {
  appendFunction <- function(this, level, message, echoToConsole) {
    # Avoid note in check:
    # Should add session id (session$token) to help group logs by session in central log database.
    # paste0("[sandbox-co2-log] session:", session$token, message)
    missing(this)
    message <- paste0("[sandbox-co2-log] --session:",sToken,"-- ",message)
    writeLines(message, con = stderr())
  }
  appender <- list(appendFunction = appendFunction, layout = layout)
  class(appender) <- "Appender"
  return(appender)
}
