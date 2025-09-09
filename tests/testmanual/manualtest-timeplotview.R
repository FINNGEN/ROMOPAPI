
options(bigQueryConnectorInsertDelay = 1)
options(bigQueryConnectorDdlDelay = 1)

tictoc::tic()
CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
tictoc::toc()

conceptId <- c(317009)  # asthma snomde
conceptId <- 21601860 # ATC level 5 
conceptId <- 45596282 # asthma ICD10
conceptId <- 21600744 + 2100000000 # ATC level 5 
conceptId <- 21600788 + 2100000000 # ATC level 5 

conceptId <- 21603409 


results <- getCodeCounts(
    CDMdbHandler,
    conceptId = conceptId
)

all <- result$concept_relationships |>
    dplyr::left_join(result$code_counts, by = c("child_concept_id" = "concept_id")) |>
    dplyr::left_join(result$concepts, by = c("child_concept_id" = "concept_id"))

all |>
    dplyr::filter(level != "-1" & level != "Mapped from") |>
    dplyr::group_by(level, concept_name, child_concept_id, calendar_year) |>
    dplyr::summarise(event_counts = sum(event_counts), .groups = "drop") |>
    dplyr::arrange(level) |> 
    dplyr::mutate(concept_lable = paste(level, ' ', concept_name)) |>
    ggplot2::ggplot(ggplot2::aes(x = calendar_year, y = event_counts, fill = as.factor(concept_lable))) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_discrete(name = "Concept Name")




all |>
    dplyr::filter(level == "Mapped from") |>
    dplyr::group_by(level, concept_name, vocabulary_id, calendar_year) |>
    dplyr::summarise(event_counts = sum(event_counts), .groups = "drop") |>
    dplyr::arrange(level) |> 
    dplyr::mutate(concept_lable = paste( concept_name, ' ', vocabulary_id)) |>
    ggplot2::ggplot(ggplot2::aes(x = calendar_year, y = event_counts, fill = as.factor(concept_lable))) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_discrete(name = "Concept Name")


all |>
    dplyr::group_by(level, child_concept_id, parent_concept_id, concept_name, vocabulary_id) |>
    dplyr::summarise(event_counts = sum(event_counts), descendant_event_counts = sum(descendant_event_counts), .groups = "drop")  |> 
    print(n = Inf)


all |>
    #dplyr::filter(level != "Parent" & level != "Mapped from") |>
    dplyr::group_by(level, concept_name, child_concept_id) |>
    dplyr::summarise(event_counts = sum(event_counts), .groups = "drop") |> 
    print(n = Inf)




createMermaidGraphFromResults(results) |> cat()


createCodeCountsTableFromResults(results)

createPlotFromResults(results)
