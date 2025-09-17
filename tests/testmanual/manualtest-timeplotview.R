
options(bigQueryConnectorInsertDelay = 1)
options(bigQueryConnectorDdlDelay = 1)

tictoc::tic()
CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
tictoc::toc()

conceptId <- c(317009)  # asthma snomde
conceptId <- 21601860 # ATC level 5 
conceptId <- 45596282 # asthma ICD10

conceptId <- 21601855       # ATC C10AA
conceptId <- 21601823
conceptId <- 21601487
conceptId <- 782748

conceptId <- 21602735


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
# create report
report.html <- createReport(conceptId, CDMdbHandler, showsMappings = FALSE)
browseURL(report.html)

report.html <- createReport(conceptId, CDMdbHandler, showsMappings = FALSE, pruneLevels = 2)
browseURL(report.html)



#
# EXAMPLES
#

# ATC level 4: ATC C10AA

conceptId <- 21601855 

results <- getCodeCounts(
    CDMdbHandler,
    conceptId = conceptId
)

createMermaidGraphFromResults(results) |> clipr::write_clip()

report.html <- createReport(conceptId, CDMdbHandler, showsMappings = FALSE)
browseURL(report.html)

## Cut to level 2
results2 <- pruneLevelsFromResults(results, pruneLevels = 2)

createMermaidGraphFromResults(results2) |> clipr::write_clip()

report.html <- createReport(conceptId, CDMdbHandler, showsMappings = FALSE, pruneLevels = 2)
browseURL(report.html)

## Cut to level 2 and remove ingredients
results3 <- pruneLevelsFromResults(results, pruneLevels = 2, pruneClass = "Ingredient")
createMermaidGraphFromResults(results3) |> clipr::write_clip()
report.html <- createReport(conceptId, CDMdbHandler, showsMappings = FALSE, pruneLevels = 2)
browseURL(report.html)


## Cut to level 3 
results4 <- pruneLevelsFromResults(results, pruneLevels = 3)
createMermaidGraphFromResults(results4) |> clipr::write_clip()
report.html <- createReport(conceptId, CDMdbHandler, showsMappings = FALSE, pruneLevels = 3)
browseURL(report.html)

## Cut to level 3 and remove ingredients
results5 <- pruneLevelsFromResults(results, pruneLevels = 3, pruneClass = "Ingredient")
createMermaidGraphFromResults(results5) |> clipr::write_clip()
report.html <- createReport(conceptId, CDMdbHandler, showsMappings = FALSE, pruneLevels = 3, pruneClass = "Ingredient")
browseURL(report.html)


## Cut to level 4 and remove ingredients
results5 <- pruneLevelsFromResults(results, pruneLevels = 4, pruneClass = "Ingredient")
createMermaidGraphFromResults(results5) |> clipr::write_clip()
report.html <- createReport(conceptId, CDMdbHandler, showsMappings = FALSE, pruneLevels = 3, pruneClass = "Ingredient")
browseURL(report.html)



# ATC level 5: ATC C10AA07

conceptId <- 21601862

results <- getCodeCounts(
    CDMdbHandler,
    conceptId = conceptId
)

createMermaidGraphFromResults(results) |> clipr::write_clip()
report.html <- createReport(conceptId, CDMdbHandler, showsMappings = FALSE)
browseURL(report.html)


results2 <- pruneLevelsFromResults(results, pruneLevels = 100, pruneClass = "Clinical Drug Comp")
createMermaidGraphFromResults(results2) |> clipr::write_clip()



conceptId <- 21602515

results <- getCodeCounts(
    CDMdbHandler,
    conceptId = conceptId
)
