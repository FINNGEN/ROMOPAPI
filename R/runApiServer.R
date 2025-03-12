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
        pathToFinnGenEunomiaSqlite <- helper_FinnGen_getDatabaseFile()

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

    # Run plumber router
    plumberRouter |>
        plumber::pr_run(
            host = host,
            port = port,
            ...
        )
}
