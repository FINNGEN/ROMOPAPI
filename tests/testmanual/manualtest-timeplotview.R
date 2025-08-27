CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")

conceptIds <- c(317009)  # asthma snomde
conceptIds <- 21601860 # ATC level 5 
conceptIds <- 45596282 # asthma ICD10
conceptIds <- 21600744 + 2100000000 # ATC level 5 

result <- getCodeCounts(
    CDMdbHandler,
    conceptIds = conceptIds
)


all <- result$concept_relationships |>
    dplyr::left_join(result$code_counts, by = c("concept_id_2" = "concept_id")) |>
    dplyr::left_join(result$concepts, by = c("concept_id_2" = "concept_id"))

all |>
    dplyr::filter(relationship_id != "Parent" & relationship_id != "Mapped from") |>
    dplyr::group_by(relationship_id, concept_name, concept_id_2, calendar_year) |>
    dplyr::summarise(event_counts = sum(event_counts), .groups = "drop") |>
    dplyr::arrange(relationship_id) |> 
    dplyr::mutate(concept_lable = paste(relationship_id, ' ', concept_name)) |>
    ggplot2::ggplot(ggplot2::aes(x = calendar_year, y = event_counts, fill = as.factor(concept_lable))) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_discrete(name = "Concept Name")




all |>
    dplyr::filter(relationship_id == "Mapped from") |>
    dplyr::group_by(relationship_id, concept_name, vocabulary_id, calendar_year) |>
    dplyr::summarise(event_counts = sum(event_counts), .groups = "drop") |>
    dplyr::arrange(relationship_id) |> 
    dplyr::mutate(concept_lable = paste( concept_name, ' ', vocabulary_id)) |>
    ggplot2::ggplot(ggplot2::aes(x = calendar_year, y = event_counts, fill = as.factor(concept_lable))) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_discrete(name = "Concept Name")


all |>
    dplyr::group_by(relationship_id, concept_id_1, concept_id_2, concept_name, vocabulary_id) |>
    dplyr::summarise(event_counts = sum(event_counts), .groups = "drop")  |> 
    print(n = Inf)


all |>
    #dplyr::filter(relationship_id != "Parent" & relationship_id != "Mapped from") |>
    dplyr::group_by(relationship_id, concept_name, concept_id_2) |>
    dplyr::summarise(event_counts = sum(event_counts), .groups = "drop") |> 
    print(n = Inf)
