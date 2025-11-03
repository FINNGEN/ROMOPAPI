#' Run the ROMOPAPI API server
#'
#' @description
#' Starts a Plumber API server for the ROMOPAPI package. If no database configuration
#' is provided, it automatically uses the test FinnGen Eunomia database and creates
#' necessary code counts tables.
#'
#' @param cohortTableHandlerConfig Configuration for connecting to the OMOP CDM database.
#'   If NULL, uses test Eunomia database
#' @param host Host address to run the API server on. Defaults to "127.0.0.1"
#' @param port Port number to run the API server on. Defaults to 8585
#' @param buildCountsTable Logical indicating whether to build code counts tables. Defaults to FALSE
#' @param ... Additional arguments passed to plumber::pr_run()
#'
#' @importFrom plumber pr pr_run pr_set_docs pr_set_api_spec
#' @importFrom rlang env
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run with default test database
#' runApiServer()
#'
#' # Run with custom configuration
#' config <- list(...) # your database config
#' runApiServer(cohortTableHandlerConfig = config, port = 9000)
#' }
#'
#' @note
#' The API server includes CORS support and automatic API documentation.
#' When using the test database, code counts tables are automatically created.
runApiServer <- function(
    cohortTableHandlerConfig = NULL,
    host = "127.0.0.1",
    port = 8585,
    buildCountsTable = FALSE,
    ...) {
    #
    # VALIDATE
    #

    fct_setUpLogger()
    ParallelLogger::logInfo("Starting ROMOPAPI API server")

    if (is.null(cohortTableHandlerConfig)) {
        ParallelLogger::logInfo("No path to database config provided. Using the test counts only database.")
        # if not provided, use the test counts only database
        test_databasesConfig <- HadesExtras_readAndParseYaml(
            pathToYalmFile = system.file("testdata", "config", "onlyCounts_databasesConfig.yml", package = "ROMOPAPI"),
            pathToFinnGenCountsSqlite = helper_FinnGen_getDatabaseFileCounts()
        )
       
        cohortTableHandlerConfig <- test_databasesConfig[[1]]$cohortTableHandler

        # Create CDMdbHandler
        CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
    } else {
        # Create CDMdbHandler
        CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
    }

    if (buildCountsTable == TRUE) {
        ParallelLogger::logInfo("Building code counts tables")
        createCodeCountsTables(CDMdbHandler, codeCountsTable = "code_counts")
    }

    # Create plumber router
    pathToPlumberFile <- system.file("plumber", "plumber.R", package = "ROMOPAPI")

    plumberRouter <- plumber::pr(
        file = pathToPlumberFile,
        env = rlang::env(
            CDMdbHandler = CDMdbHandler
        )
    )

    # Add CORS middleware
    plumberRouter <- plumberRouter |>
        plumber::pr_set_docs(docs = TRUE) |>
        plumber::pr_set_api_spec(function(spec) {
            spec$components$parameters <- list()
            return(spec)
        })

    # Run plumber router
    plumberRouter |>
        plumber::pr_run(
            host = host,
            port = port,
            ...
        )
}
