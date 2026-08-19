#' Create a Mermaid graph visualization from results
#' @param results List containing concept relationships and counts data
#' @param showsMappings Logical indicating whether to show concept mappings
#' @return String containing Mermaid graph definition
#' @importFrom dplyr group_by summarise left_join filter if_else mutate anti_join pull bind_rows
#' @export
#' 
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
      "RC:", record_counts, " (shown ", node_record_counts, ")<br>",
      "DRC:", descendant_record_counts, " (shown ", node_descendant_record_counts, ")<br>",
      "PC:", person_counts, " DPC:", descendant_person_counts, "<br>",
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


#' Clean concept name for Mermaid compatibility
#' @param name Character string to clean
#' @return Cleaned character string
#' @keywords internal
#' 
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



#' Create a reactable table from results
#' @param results List containing concept relationships and counts data
#' @return A reactable table object
#' @importFrom dplyr group_by summarise left_join filter select distinct rename arrange
#' @importFrom tidyr nest
#' @importFrom reactable reactable colDef
#' @export
#' 
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
      person_counts,
      descendant_person_counts,
      data
    )


  columns <- list(
    concept_id = reactable::colDef(name = "Concept ID"),
    concept_name = reactable::colDef(name = "Concept Name"),
    concept_code = reactable::colDef(name = "Concept Code"),
    vocabulary_id = reactable::colDef(name = "Vocabulary ID"),
    record_counts = reactable::colDef(name = "RC - Record Counts"),
    node_record_counts = reactable::colDef(name = "RC (shown) - Node Record Counts"),
    descendant_record_counts = reactable::colDef(name = "DRC - Descendant Record Counts"),
    node_descendant_record_counts = reactable::colDef(name = "DRC (shown) - Node Descendant Record Counts"),
    person_counts = reactable::colDef(name = "PC - Person Counts"),
    descendant_person_counts = reactable::colDef(name = "DPC - Descendant Person Counts"),
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



#' Create a plotly visualization from results
#' @param results List containing concept relationships and counts data
#' @param showsMappings Logical indicating whether to show concept mappings
#' @param ... Additional arguments passed to plotly::ggplotly
#' @return A plotly object
#' @importFrom dplyr group_by summarise left_join filter mutate case_when rename distinct arrange bind_rows select pull inner_join
#' @importFrom ggplot2 ggplot aes geom_area geom_line theme_minimal scale_fill_manual theme
#' @importFrom plotly ggplotly layout
#' @export
#' 
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

#' Convert concept ID to RGB color
#' @param conceptId Numeric concept ID
#' @return Character string with hex color code
#' @importFrom digest digest
#' @keywords internal
#' 
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



#' Convert a gender concept ID to a display label
#' @param genderConceptId Integer OMOP gender concept ID (8507 male, 8532 female)
#' @return Character label
#' @importFrom dplyr case_when
#' @keywords internal
#'
.genderConceptIdToLabel <- function(genderConceptId) {
  dplyr::case_when(
    genderConceptId == 8507 ~ "Male",
    genderConceptId == 8532 ~ "Female",
    TRUE ~ paste0("Unknown (", genderConceptId, ")")
  )
}

#' Create a pie chart of person counts by sex
#'
#' @param filterPersonCounts The `filter_person_counts` tibble from \code{\link{getPersonCounts}}
#'
#' @return A plotly object
#' @importFrom dplyr filter mutate
#' @importFrom plotly plot_ly layout
#' @export
#'
createSexPieChartFromPersonCounts <- function(filterPersonCounts) {
  data <- filterPersonCounts |>
    dplyr::filter(filter == "sex") |>
    dplyr::mutate(label = .genderConceptIdToLabel(stratum))

  plotly::plot_ly(data, labels = ~label, values = ~person_counts, type = "pie") |>
    plotly::layout(title = "Persons by sex")
}

#' Create a bar chart of person counts by age decile
#'
#' @param filterPersonCounts The `filter_person_counts` tibble from \code{\link{getPersonCounts}}
#'
#' @return A plotly object
#' @importFrom dplyr filter arrange mutate
#' @importFrom ggplot2 ggplot aes geom_col theme_minimal labs
#' @importFrom plotly ggplotly
#' @export
#'
createAgeHistogramFromPersonCounts <- function(filterPersonCounts) {
  data <- filterPersonCounts |>
    dplyr::filter(filter == "age") |>
    dplyr::arrange(stratum) |>
    dplyr::mutate(label = paste0(stratum * 10, "-", stratum * 10 + 9))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = factor(label, levels = label), y = person_counts)) +
    ggplot2::geom_col(fill = "#4477AA") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Age decile", y = "Persons", title = "Persons by age")

  plotly::ggplotly(plot)
}

#' Create a bar chart of person counts by visit-source group
#'
#' @param filterPersonCounts The `filter_person_counts` tibble from \code{\link{getPersonCounts}}
#' @param visitTypeNames Optional tibble from \code{\link{getVisitTypeNames}} (columns
#'   `visit_group_concept_id`, `concept_name`) used to label the groups. NULL (default)
#'   labels by the raw `visit_group_concept_id`.
#'
#' @return A plotly object
#' @importFrom dplyr filter rename left_join mutate coalesce
#' @importFrom ggplot2 ggplot aes geom_col theme_minimal theme element_text labs
#' @importFrom plotly ggplotly
#' @export
#'
createVisitBarplotFromPersonCounts <- function(filterPersonCounts, visitTypeNames = NULL) {
  data <- filterPersonCounts |>
    dplyr::filter(filter == "visit") |>
    dplyr::rename(visit_group_concept_id = stratum)

  if (!is.null(visitTypeNames)) {
    data <- data |>
      dplyr::left_join(visitTypeNames, by = "visit_group_concept_id") |>
      dplyr::mutate(label = dplyr::coalesce(concept_name, paste0("Visit group ", visit_group_concept_id)))
  } else {
    data <- data |>
      dplyr::mutate(label = paste0("Visit group ", visit_group_concept_id))
  }

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = label, y = person_counts)) +
    ggplot2::geom_col(fill = "#CC6677") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "Visit source group", y = "Persons", title = "Persons by visit source group")

  plotly::ggplotly(plot)
}

#' Create an UpSet-style plot of exact set-overlap regions
#'
#' @description
#' Renders the exact exclusive-region person counts from
#' \code{\link{getPersonCounts}}'s `upset_person_counts` as a bar chart of region sizes
#' (top) stacked on a set-membership matrix (bottom), sharing the region ordering on the
#' x-axis — the standard UpSet plot layout.
#'
#' @param upsetPersonCounts The `upset_person_counts` tibble from \code{\link{getPersonCounts}}
#'   (columns `group`, `person_counts`)
#' @param concepts Optional tibble with `concept_id`/`concept_name` (e.g.
#'   \code{\link{getCodeCounts}}'s `concepts`) used to label the sets. NULL (default)
#'   labels by the raw concept ID.
#'
#' @return A plotly object
#' @importFrom dplyr arrange desc mutate group_by summarise pull filter left_join select coalesce
#' @importFrom tidyr unnest
#' @importFrom plotly plot_ly add_markers add_segments subplot layout
#' @export
#'
createUpsetPlotFromPersonCounts <- function(upsetPersonCounts, concepts = NULL) {
  regions <- upsetPersonCounts |>
    dplyr::arrange(dplyr::desc(person_counts)) |>
    dplyr::mutate(region = factor(group, levels = group))

  membership <- regions |>
    dplyr::mutate(concept_id = strsplit(group, "-")) |>
    tidyr::unnest(concept_id) |>
    dplyr::mutate(concept_id = as.integer(concept_id))

  if (!is.null(concepts)) {
    membership <- membership |>
      dplyr::left_join(dplyr::select(concepts, concept_id, concept_name), by = "concept_id") |>
      dplyr::mutate(set_label = dplyr::coalesce(concept_name, paste0("Concept ", concept_id)))
  } else {
    membership <- membership |>
      dplyr::mutate(set_label = paste0("Concept ", concept_id))
  }

  setLevels <- membership |>
    dplyr::group_by(set_label) |>
    dplyr::summarise(total = sum(person_counts), .groups = "drop") |>
    dplyr::arrange(total) |>
    dplyr::pull(set_label)

  membership <- membership |>
    dplyr::mutate(set_pos = match(set_label, setLevels))

  segments <- membership |>
    dplyr::group_by(region) |>
    dplyr::summarise(y0 = min(set_pos), y1 = max(set_pos), .groups = "drop") |>
    dplyr::filter(y0 != y1)

  barPlot <- plotly::plot_ly(regions, x = ~region, y = ~person_counts, type = "bar", marker = list(color = "#4477AA")) |>
    plotly::layout(yaxis = list(title = "Persons (exclusive)"))

  matrixPlot <- plotly::plot_ly()
  if (nrow(segments) > 0) {
    matrixPlot <- matrixPlot |>
      plotly::add_segments(
        data = segments, x = ~region, xend = ~region, y = ~y0, yend = ~y1,
        line = list(color = "#333333"), showlegend = FALSE
      )
  }
  matrixPlot <- matrixPlot |>
    plotly::add_markers(
      data = membership, x = ~region, y = ~set_pos,
      marker = list(color = "#333333", size = 10), showlegend = FALSE
    ) |>
    plotly::layout(
      yaxis = list(
        title = "", tickvals = seq_along(setLevels), ticktext = setLevels,
        range = c(0.5, length(setLevels) + 0.5)
      ),
      xaxis = list(title = "Region")
    )

  plotly::subplot(barPlot, matrixPlot, nrows = 2, shareX = TRUE, heights = c(0.6, 0.4))
}

#' Prune levels from results
#'
#' @param results A list of results from getCodeCounts
#' @param pruneLevels The levels to prune
#' @param pruneClass Character string specifying a concept class to filter results.
#'   NULL includes all concept classes. Common values include "Ingredient", "Clinical Drug", etc.
#'
#' @return A list of results with the levels pruned
#' @importFrom dplyr mutate if_else filter select pull
#' @importFrom stringr str_detect str_sub
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