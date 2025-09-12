
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



createMermaidGraphFromResults(results, showsMappings = TRUE) |> clipr::write_clip()
createCodeCountsTableFromResults(results)
createPlotFromResults(results, showsMappings = TRUE)



# remove levels
pruneLevels <- 2
results <- pruneLevelsFromResults(results, pruneLevels, pruneClass = "Ingredient")



# create report
report.html <- createReport(conceptId, CDMdbHandler, showsMappings = TRUE, pruneLevels = 2)
browseURL(report.html)
