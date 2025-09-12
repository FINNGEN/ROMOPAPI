
options(bigQueryConnectorInsertDelay = 1)
options(bigQueryConnectorDdlDelay = 1)

tictoc::tic()
CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
tictoc::toc()

conceptId <- c(317009)  # asthma snomde
conceptId <- 21601860 # ATC level 5 
conceptId <- 45596282 # asthma ICD10

conceptId <- 21601855 
conceptId <- 21601823
conceptId <- 21601487


results <- getCodeCounts(
    CDMdbHandler,
    conceptId = conceptId
)


createMermaidGraphFromResults(results) |> clipr::write_clip()

createCodeCountsTableFromResults(results)

createPlotFromResults(results)

createPlotFromResults(results, showsMappings = TRUE)

