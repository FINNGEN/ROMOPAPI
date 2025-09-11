
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

results$concept_relationships |>
    dplyr::left_join(results$code_counts, by = c("child_concept_id" = "concept_id")) |>
    dplyr::left_join(results$concepts, by = c("child_concept_id" = "concept_id"))|>
    #dplyr::filter(level != "Parent" & level != "Mapped from") |>
    dplyr::group_by(level, concept_name, concept_code, child_concept_id) |>
    dplyr::summarise(record_counts = sum(record_counts), descendant_record_counts = sum(descendant_record_counts), .groups = "drop") |> 
    View()



createMermaidGraphFromResults(results) |> cat()

createCodeCountsTableFromResults(results)

createPlotFromResults(results)

