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
    ...) {
    #
    # VALIDATE
    #

    if (is.null(cohortTableHandlerConfig)) {
        message("No path to database config provided. Using the test FinnGen Eunomia database.")
        # if not provided, use the test FinnGen Eunomia database
        pathToFinnGenEunomiaSqlite <- helper_FinnGen_getDatabaseFile(counts = TRUE)

        databasesConfig <- HadesExtras::readAndParseYaml(
            pathToYalmFile = system.file("testdata", "config", "eunomia_databasesConfig.yml", package = "ROMOPAPI"),
            pathToGiBleedEunomiaSqlite = "",
            pathToMIMICEunomiaSqlite = "",
            pathToFinnGenEunomiaSqlite = pathToFinnGenEunomiaSqlite
        )

        cohortTableHandlerConfig <- databasesConfig[[4]]$cohortTableHandle

        # Create CDMdbHandler
        CDMdbHandler  <- HadesExtras::createCDMdbHandlerFromList(cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")

        # Create code counts table
        message("Creating code counts table")
        createCodeCountsTable(CDMdbHandler)
 
    } else {
        # Create CDMdbHandler
        CDMdbHandler  <- HadesExtras::createCDMdbHandlerFromList(cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
    }

    # Create plumber router
    pathToPlumberFile <- system.file("plumber", "plumber.R", package = "ROMOPAPI")

    plumberRouter  <- plumber::pr(
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
