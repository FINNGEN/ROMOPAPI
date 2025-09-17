createMermaidGraphFromResults <- function(
    results,
    showsMappings = FALSE) {
  concept_relationships <- results$concept_relationships
  concepts <- results$concepts

  aggregated_counts <- results$stratified_code_counts |>
    dplyr::group_by(concept_id) |>
    dplyr::summarise(node_record_counts = sum(node_record_counts), node_descendant_record_counts = sum(node_descendant_record_counts), .groups = "drop")

  concepts <- concepts |>
    dplyr::left_join(aggregated_counts, by = "concept_id")

  if (nrow(concept_relationships) == 0) {
    return("graph TD\n    A[No relationships found]")
  }


  # map relationships as subgraphs
  subgrapPrefix <- ""
  map_code <- ""
  if (showsMappings) {
    subgrapPrefix <- "s"

    mappingLines <- concept_relationships |>
      dplyr::filter(levels %in% c("Mapped from", "Maps to")) |>
      dplyr::mutate(
        line = dplyr::if_else(levels == "Maps to", paste0(parent_concept_id, " --> ", child_concept_id), paste0(child_concept_id, " --> ", parent_concept_id))
      ) |>
      dplyr::group_by(parent_concept_id) |>
      dplyr::summarise(
        subgraph_code =
          paste0(line, collapse = "\n"),
        .groups = "drop"
      )

    # add single nodes
    noMappingsLines <- concept_relationships |>
      dplyr::filter(!(levels %in% c("Mapped from", "Maps to"))) |>
      dplyr::select(parent_concept_id = child_concept_id) |>
      dplyr::anti_join(mappingLines, by = "parent_concept_id") |>
      dplyr::mutate(subgraph_code = paste0(parent_concept_id))

    mappingLines <- dplyr::bind_rows(mappingLines, noMappingsLines)

    map_code <- mappingLines |>
      dplyr::mutate(subgraph_code = paste0("subgraph ", subgrapPrefix, parent_concept_id, "\n", subgraph_code, "\nend")) |>
      dplyr::pull(subgraph_code) |>
      paste(collapse = "\n")
  }

  # tree relationships
  tree_code <- concept_relationships |>
    # remove levels '0' and reverse 'levels'  -1
    dplyr::filter(levels != "0") |>
    dplyr::mutate(tmp_child_concept_id = child_concept_id) |>
    dplyr::mutate(child_concept_id = dplyr::if_else(levels == "-1", parent_concept_id, child_concept_id)) |>
    dplyr::mutate(parent_concept_id = dplyr::if_else(levels == "-1", tmp_child_concept_id, parent_concept_id)) |>
    #
    dplyr::select(-tmp_child_concept_id) |>
    dplyr::filter(!(levels %in% c("Maps to", "Mapped from"))) |>
    dplyr::mutate(line = paste0(subgrapPrefix, parent_concept_id, " --> ", subgrapPrefix, child_concept_id)) |>
    dplyr::pull(line) |>
    unique() |>
    paste(collapse = "\n")

  # nodes
  if (!showsMappings) {
    concept_relationships <- concept_relationships |>
      dplyr::filter(!(levels %in% c("Maps to", "Mapped from")))
  }
  concept_ids <- unique(concept_relationships$child_concept_id)

  edges_code <- concepts |>
    dplyr::filter(concept_id %in% concept_ids) |>
    dplyr::mutate(line = paste0(
      concept_id, "[\"",
      .cleanConceptNameForMermaid(concept_name), "\"<br>",
      concept_code, "<br>",
      vocabulary_id, "<br>",
      record_counts, "-", node_record_counts, "<br>",
      descendant_record_counts, "-", node_descendant_record_counts, "<br>",
      concept_class_id,
      "]"
    )) |>
    dplyr::pull(line) |>
    unique() |>
    paste(collapse = "\n")

  color_code <- concepts |>
    dplyr::filter(concept_id %in% concept_ids) |>
    dplyr::mutate(rgb = sapply(concept_id, .conceptIdToRGB)) |>
    dplyr::mutate(line = paste0("style ", concept_id, " stroke:", rgb, ",stroke-width:4px")) |>
    dplyr::pull(line) |>
    unique() |>
    paste(collapse = "\n")

  mermaid_code <- paste0("flowchart TB", "\n\n", map_code, "\n\n", tree_code, "\n\n", edges_code, "\n\n", color_code, "\n\n")

  return(mermaid_code)
}


.cleanConceptNameForMermaid <- function(name) {
  # Clean concept name for Mermaid compatibility
  # Escape special characters instead of removing them

  # Escape quotes by doubling them
  name <- gsub('"', '""', name)

  # Replace problematic characters with safe alternatives
  name <- gsub("\\[", "(", name) # Replace [ with (
  name <- gsub("\\]", ")", name) # Replace ] with )
  name <- gsub("\\{", "(", name) # Replace { with (
  name <- gsub("\\}", ")", name) # Replace } with )
  name <- gsub("\\|", "-", name) # Replace | with -
  name <- gsub("&", "and", name) # Replace & with 'and'

  # Normalize whitespace
  name <- gsub("\\s+", " ", name) # Replace multiple spaces with single space
  name <- trimws(name) # Remove leading/trailing whitespace

  return(name)
}



createCodeCountsTableFromResults <- function(results) {
  concept_relationships <- results$concept_relationships
  concepts <- results$concepts

  aggregated_counts <- results$stratified_code_counts |>
    dplyr::group_by(concept_id) |>
    dplyr::summarise(node_record_counts = sum(node_record_counts), node_descendant_record_counts = sum(node_descendant_record_counts), .groups = "drop")

  concepts <- concepts |>
    dplyr::left_join(aggregated_counts, by = "concept_id")


  treeTable <- concept_relationships |>
    dplyr::filter(!(levels %in% c("Mapped from", "Maps to"))) |>
    dplyr::select(-parent_concept_id, -concept_class_id) |>
    dplyr::distinct(child_concept_id, .keep_all = TRUE) |>
    dplyr::rename(concept_id = child_concept_id) |>
    dplyr::left_join(concepts, by = "concept_id") |>
    dplyr::arrange(levels)


  mappingTable <- concept_relationships |>
    dplyr::filter(levels %in% c("Mapped from", "Maps to")) |>
    dplyr::select(parent_concept_id, concept_id = child_concept_id) |>
    dplyr::left_join(aggregated_counts, by = "concept_id") |>
    dplyr::left_join(concepts, by = "concept_id") |>
    tidyr::nest(data = -parent_concept_id)

  table <- treeTable |>
    dplyr::left_join(mappingTable, by = c("concept_id" = "parent_concept_id")) |>
    dplyr::select(
      levels,
      concept_id,
      concept_name,
      concept_code,
      vocabulary_id,
      record_counts,
      node_record_counts,
      descendant_record_counts,
      node_descendant_record_counts,
      data
    )


  columns <- list(
    concept_id = reactable::colDef(name = "Concept ID"),
    concept_name = reactable::colDef(name = "Concept Name"),
    concept_code = reactable::colDef(name = "Concept Code"),
    vocabulary_id = reactable::colDef(name = "Vocabulary ID"),
    record_counts = reactable::colDef(name = "Event Counts"),
    node_record_counts = reactable::colDef(name = "Node Event Counts"),
    descendant_record_counts = reactable::colDef(name = "Descendant Event Counts"),
    node_descendant_record_counts = reactable::colDef(name = "Node Descendant Event Counts"),
    data = reactable::colDef(name = "Data", show = FALSE)
  )


  details <- function(index) {
    data <- table$data[[index]]
    if (is.null(data)) {
      return(NULL)
    }
    data <- data |>
      dplyr::select(concept_id, concept_name, concept_code, vocabulary_id, record_counts, descendant_record_counts)
    reactable::reactable(data, fullWidth = FALSE)
  }

  table |>
    reactable::reactable(
      columns = columns,
      details = details
    )
}



createPlotFromResults <- function(results, showsMappings = FALSE, ...) {
  parentConceptIds <- results$concept_relationships |>
    dplyr::filter(!levels %in% c("Mapped from", "Maps to")) |>
    dplyr::pull(parent_concept_id) |>
    unique()

  conceptList <- results$concept_relationships |>
    dplyr::filter(!levels %in% c("-1", "Maps to")) |>
    dplyr::mutate(isLeaf = dplyr::case_when(
      levels == "Mapped from" ~ as.logical(NA),
      child_concept_id %in% parentConceptIds ~ FALSE,
      TRUE ~ TRUE
    )) |>
    dplyr::rename(concept_id = child_concept_id) |>
    dplyr::distinct(concept_id, levels, isLeaf)

  stratified_code_counts <- results$stratified_code_counts
  concepts <- results$concepts

  timeCounts <- stratified_code_counts |>
    dplyr::group_by(concept_id, calendar_year) |>
    dplyr::summarise(node_record_counts = sum(node_record_counts), node_descendant_record_counts = sum(node_descendant_record_counts), .groups = "drop")

  all <- conceptList |>
    dplyr::left_join(timeCounts, by = c("concept_id" = "concept_id")) |>
    dplyr::left_join(concepts, by = c("concept_id" = "concept_id")) |>
    dplyr::arrange(levels) |>
    dplyr::mutate(
      concept_lable = paste(levels, " ", concept_name),
      rgb = sapply(concept_id, .conceptIdToRGB)
    )


  if (showsMappings) {
    stackAreasData <- all |>
      dplyr::filter(levels == "Mapped from") |>
      dplyr::select(concept_id, calendar_year, record_counts = node_record_counts, concept_lable, rgb)
  } else {
    stackAreasData <- dplyr::bind_rows(
      all |>
        dplyr::filter(levels != "Mapped from") |>
        dplyr::filter(isLeaf) |>
        dplyr::select(concept_id, calendar_year, record_counts = node_descendant_record_counts, concept_lable, rgb),
      all |>
        dplyr::filter(levels != "Mapped from") |>
        dplyr::filter(!isLeaf) |>
        dplyr::select(concept_id, calendar_year, record_counts = node_record_counts, concept_lable, rgb)
    )
  }

  lineData <- all |>
    dplyr::filter(levels == "0") |>
    dplyr::select(concept_id, calendar_year, node_descendant_record_counts, concept_lable, rgb)

  # debug
  message("sum of lineData: ", lineData |> pull(node_descendant_record_counts) |> sum())
  message("sum of stackAreasData: ", stackAreasData |> pull(record_counts) |> sum())

  a <- stackAreasData |>
    dplyr::group_by(calendar_year) |>
    dplyr::summarise(record_counts = sum(record_counts))
  b <- lineData |>
    dplyr::group_by(calendar_year) |>
    dplyr::summarise(node_descendant_record_counts = sum(node_descendant_record_counts))
  c <- dplyr::inner_join(a, b, by = "calendar_year") |>
    dplyr::mutate(diff = record_counts - node_descendant_record_counts) |>
    dplyr::pull(diff) |>
    sum()
  message("time diff: ", c)
  # end debug

  plot <-
    ggplot2::ggplot(
      data = stackAreasData,
      ggplot2::aes(x = calendar_year, y = record_counts, fill = concept_lable)
    ) +
    ggplot2::geom_area(stat = "identity", position = "stack") +
    ggplot2::geom_line(
      data = lineData,
      ggplot2::aes(x = calendar_year, y = node_descendant_record_counts),
      color = "black",
      size = 1
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = setNames(all$rgb, all$concept_lable)) +
    ggplot2::theme(legend.position = "top")

  plotly::ggplotly(plot, ...) |>
    plotly::layout(legend = list(orientation = "h", y = 1.1))
}

.conceptIdToRGB <- function(conceptId) {
  # get the hash of the conceptId
  hash <- digest::digest(conceptId, algo = "sha256")

  # Use different parts of the hash to create more distinct colors
  # Take 2 characters each from different positions to ensure variety
  r_hex <- substr(hash, 1, 2)
  g_hex <- substr(hash, 7, 8) # Use different position
  b_hex <- substr(hash, 13, 14) # Use different position

  # Ensure minimum brightness by setting minimum values
  r_val <- max(as.numeric(paste0("0x", r_hex)), 50)
  g_val <- max(as.numeric(paste0("0x", g_hex)), 50)
  b_val <- max(as.numeric(paste0("0x", b_hex)), 50)

  # Convert back to hex with padding
  r_hex <- sprintf("%02X", r_val)
  g_hex <- sprintf("%02X", g_val)
  b_hex <- sprintf("%02X", b_val)

  hex <- paste0("#", r_hex, g_hex, b_hex)

  return(hex)
}



#' Prune levels from results
#'
#' @param results A list of results from getCodeCounts
#' @param pruneLevels The levels to prune
#' @param pruneClass Character string specifying a concept class to filter results.
#'   NULL includes all concept classes. Common values include "Ingredient", "Clinical Drug", etc.
#'
#' @return A list of results with the levels pruned
#' @export
#' 
pruneLevelsFromResults <- function(results, pruneLevels, pruneClass = NULL) {

  concept_relationships <- results$concept_relationships
  stratified_code_counts <- results$stratified_code_counts
  concepts <- results$concepts

  concept_relationships <- concept_relationships |>
    dplyr::mutate(min_level = dplyr::if_else(
        stringr::str_detect(levels, "(\\d+)-(\\d+)"), 
        stringr::str_sub(levels, 1, 1) |> as.integer(), 
        0L)) |>
    dplyr::filter(min_level < pruneLevels) |> 
    dplyr::select(-min_level)

  if (!is.null(pruneClass)) {
    conceptIds <- concepts |>
      dplyr::filter(concept_class_id %in% pruneClass) |>
      dplyr::pull(concept_id)
    concept_relationships <- concept_relationships |>
      dplyr::filter(!parent_concept_id %in% {{conceptIds}}) |> 
      dplyr::filter(!child_concept_id %in% {{conceptIds}})
  }


  # remove mapps to with no parent
  concept_relationships <- concept_relationships |>
    dplyr::filter(! (levels %in% c("Mapped from", "Maps to") & !parent_concept_id %in% concept_relationships$child_concept_id))

  stratified_code_counts <- stratified_code_counts |>
    dplyr::filter(concept_id %in% concept_relationships$child_concept_id)

  concepts <- concepts |>
    dplyr::filter(concept_id %in% concept_relationships$child_concept_id)

  return(list(concept_relationships = concept_relationships, stratified_code_counts = stratified_code_counts, concepts = concepts))


}