#' Get FinnGen Eunomia database file
#'
#' @description
#' Downloads and extracts the FinnGen Eunomia database if it doesn't exist locally.
#' Copies the database to a temporary directory and returns the path. Optionally
#' creates a counts database with aggregated statistics.
#'
#' @param counts Logical. If TRUE, creates a counts database with aggregated statistics.
#'   Defaults to FALSE.
#'
#' @return Path to the FinnGen Eunomia SQLite database file (temporary copy)
#'
#' @importFrom Eunomia extractLoadData
#' @importFrom utils download.file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get basic database file
#' db_path <- helper_FinnGen_getDatabaseFile()
#'
#' # Get database file with counts table
#' db_path_with_counts <- helper_FinnGen_getDatabaseFile(counts = TRUE)
#' }
#'
#' @note
#' This function requires the `EUNOMIA_DATA_FOLDER` environment variable to be set
#' to the path of the Eunomia data folder.
helper_FinnGen_getDatabaseFile <- function(counts = FALSE) {
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  urlToFinnGenEunomiaZip <- "https://raw.githubusercontent.com/FINNGEN/EunomiaDatasets/main/datasets/FinnGenR12/FinnGenR12_v5.4.zip"
  eunomiaDataFolder <- Sys.getenv("EUNOMIA_DATA_FOLDER")

  pathToDatabase <- file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite")
  pathToDatabaseCounts <- file.path(eunomiaDataFolder, "FinnGenR12_v5.4_counts.sqlite")

  # Download the database if it doesn't exist
  if (!file.exists(file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip")) |
    !file.exists(file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"))) {
    result <- utils::download.file(
      url = urlToFinnGenEunomiaZip,
      destfile = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip"),
      mode = "wb"
    )

    Eunomia::extractLoadData(
      from = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip"),
      to = pathToDatabase,
      cdmVersion = "5.4",
      verbose = TRUE
    )
  }

  # make a copy if counts database doesn't exist
  if (!file.exists(pathToDatabaseCounts) & counts) {
    file.copy(
      from = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"),
      to = pathToDatabaseCounts,
      overwrite = TRUE
    )
    # create code counts table
    connectionList <- list(
      database = list(
        databaseId = "F1",
        databaseName = "FinnGen",
        databaseDescription = "Eunomia database FinnGen"
      ),
      connection = list(
        connectionDetailsSettings = list(
          dbms = "sqlite",
          server = pathToDatabaseCounts
        )
      ),
      cdm = list(
        cdmDatabaseSchema = "main",
        vocabularyDatabaseSchema = "main"
      ),
      cohortTable = list(
        cohortDatabaseSchema = "main",
        cohortTableName = "test_cohort_table_<timestamp>"
      )
    )
    CDMdbHandler <- HadesExtras::createCDMdbHandlerFromList(connectionList, loadConnectionChecksLevel = "basicChecks")
    createCodeCountsTable(CDMdbHandler)
    CDMdbHandler$finalize()
  }

  if (counts) {
    pathFromCopyDatabase <- pathToDatabaseCounts
  } else {
    pathFromCopyDatabase <- pathToDatabase
  }

  # copy to a temp folder
  pathToCopyDatabase <- file.path(tempdir(), "FinnGenR12_v5.4.sqlite")
  file.copy(
    from = pathFromCopyDatabase,
    to = pathToCopyDatabase,
    overwrite = TRUE
  )

  return(pathToCopyDatabase)
}




createMermaidGraphFromResults <- function(
    results,
    showsMappings = TRUE) {
  concept_relationships <- results$concept_relationships
  concepts <- results$concepts
  aggregated_counts <- results$code_counts |>
    dplyr::group_by(concept_id) |>
    dplyr::summarise(record_counts = sum(record_counts), descendant_record_counts = sum(descendant_record_counts), .groups = "drop")

  if (nrow(concept_relationships) == 0) {
    return("graph TD\n    A[No relationships found]")
  }

  # remove level '0' and reverse 'level'  -1
  concept_relationships <- concept_relationships |>
    dplyr::filter(level != "0") |>
    dplyr::mutate(tmp_child_concept_id = child_concept_id) |>
    dplyr::mutate(child_concept_id = dplyr::if_else(level == "-1", parent_concept_id, child_concept_id)) |>
    dplyr::mutate(parent_concept_id = dplyr::if_else(level == "-1", tmp_child_concept_id, parent_concept_id)) |>
    dplyr::select(-tmp_child_concept_id)

  # map relationships as subgraphs
  subgrapPrefix <- ""
  map_code <- ""
  if (showsMappings) {
    subgrapPrefix <- "s"

    mappingLines <- concept_relationships |>
      dplyr::filter(level %in% c("Mapped from", "Maps to")) |>
      dplyr::mutate(
        line = dplyr::if_else(level == "Maps to", paste0(parent_concept_id, " --> ", child_concept_id), paste0(child_concept_id, " --> ", parent_concept_id))
      ) |>
      dplyr::group_by(parent_concept_id) |>
      dplyr::summarise(
        subgraph_code =
          paste0(line, collapse = "\n"),
        .groups = "drop"
      )

    # add single nodes
    noMappingsLines <- concept_relationships |>
      dplyr::filter(!(level %in% c("Mapped from", "Maps to"))) |>
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
    dplyr::filter(!(level %in% c("Maps to", "Mapped from"))) |>
    dplyr::mutate(line = paste0(subgrapPrefix, parent_concept_id, " --> ", subgrapPrefix, child_concept_id)) |>
    dplyr::pull(line) |>
    unique() |>
    paste(collapse = "\n")

  # nodes
  if (!showsMappings) {
    concept_relationships <- concept_relationships |>
      dplyr::filter(!(level %in% c("Maps to", "Mapped from")))
  }
  concept_ids <- unique(c(concept_relationships$parent_concept_id, concept_relationships$child_concept_id))

  edges_code <- concepts |>
    dplyr::filter(concept_id %in% concept_ids) |>
    dplyr::left_join(aggregated_counts, by = "concept_id") |>
    dplyr::mutate(line = paste0(concept_id, "[\"", 
    .cleanConceptNameForMermaid(concept_name), "\"<br>",
     concept_code, "<br>", 
     vocabulary_id, "<br>", 
     record_counts, "<br>", 
     descendant_record_counts, "<br>",
     concept_class_id,
     "]")) |>
    dplyr::pull(line) |>
    unique() |>
    paste(collapse = "\n")

  color_code <- concepts |>
    dplyr::filter(concept_id %in% concept_ids) |>
    dplyr::mutate(rgb = sapply(concept_id, .conceptIdToRGB)) |>
    dplyr::mutate(line = paste0("style ", concept_id, " stroke:",rgb,",stroke-width:4px")) |>
    dplyr::pull(line) |>
    unique() |>
    paste(collapse = "\n")

  mermaid_code <- paste0("flowchart TB", "\n\n", map_code, "\n\n", tree_code, "\n\n", edges_code, "\n\n", color_code)

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
  aggregated_counts <- results$code_counts |>
    dplyr::group_by(concept_id) |>
    dplyr::summarise(record_counts = sum(record_counts), descendant_record_counts = sum(descendant_record_counts), .groups = "drop")


  treeTable <- concept_relationships |>
    dplyr::filter(!(level %in% c("Mapped from", "Maps to"))) |>
    dplyr::group_by(child_concept_id) |>
    dplyr::summarise(
      order = min(level),
      level = paste0(min(level), "-", max(level)),
      n_parents = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      level = dplyr::case_when(
        level == "0-0" ~ "Root",
        level == "-1--1" ~ "Parent",
        TRUE ~ level
      )
    ) |>
    dplyr::arrange(order) |>
    dplyr::rename(concept_id = child_concept_id) |>
    dplyr::left_join(aggregated_counts, by = "concept_id") |>
    dplyr::left_join(concepts, by = "concept_id")


  mappingTable <- concept_relationships |>
    dplyr::filter(level %in% c("Mapped from", "Maps to")) |>
    dplyr::select(parent_concept_id, concept_id = child_concept_id) |>
    dplyr::left_join(aggregated_counts, by = "concept_id") |>
    dplyr::left_join(concepts, by = "concept_id") |>
    tidyr::nest(data = -parent_concept_id)

  table <- treeTable |>
    dplyr::left_join(mappingTable, by = c("concept_id" = "parent_concept_id")) |>
    dplyr::select(
      level,
      n_parents,
      concept_id,
      concept_name,
      concept_code,
      vocabulary_id,
      record_counts,
      descendant_record_counts,
      data
    )


  columns <- list(
    concept_id = reactable::colDef(name = "Concept ID"),
    concept_name = reactable::colDef(name = "Concept Name"),
    concept_code = reactable::colDef(name = "Concept Code"),
    vocabulary_id = reactable::colDef(name = "Vocabulary ID"),
    record_counts = reactable::colDef(name = "Event Counts"),
    descendant_record_counts = reactable::colDef(name = "Descendant Event Counts"),
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



createPlotFromResults <- function(results, showsMappings = FALSE) {

  if (!showsMappings) {
    concept_relationships <- results$concept_relationships  |> 
    dplyr::filter(level != "-1" & level != "Mapped from")  |> 
    dplyr::select(concept_id = child_concept_id, level)
  } else {
    concept_relationships <- results$concept_relationships  |> 
    dplyr::filter(level == "Mapped from")  |> 
    dplyr::select(concept_id = child_concept_id, level)
  }

    
  all <- concept_relationships|>
      dplyr::left_join(results$code_counts, by = c("concept_id" = "concept_id")) |>
      dplyr::left_join(results$concepts, by = c("concept_id" = "concept_id"))|>
      dplyr::group_by(level, concept_name, concept_id, calendar_year) |>
      dplyr::summarise(record_counts = sum(record_counts), .groups = "drop") |>
      dplyr::arrange(level) |> 
      dplyr::mutate(
        concept_lable = paste(level, ' ', concept_name),
        rgb = sapply(concept_id, .conceptIdToRGB)
      ) 

  plot <- all |>
      ggplot2::ggplot(ggplot2::aes(x = calendar_year, y = record_counts, fill = concept_lable)) +
      ggplot2::geom_area(position = "stack") +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_manual(values = setNames(all$rgb, all$concept_lable)) +
      ggplot2::theme(legend.position = "top")

  plotly::ggplotly(plot) |>
    plotly::layout(legend = list(orientation = "h", y = 1.1))


}

.conceptIdToRGB <- function(conceptId) {
  # get the hash of the conceptId
  hash <- digest::digest(conceptId, algo = "sha256")
  
  # Use different parts of the hash to create more distinct colors
  # Take 2 characters each from different positions to ensure variety
  r_hex <- substr(hash, 1, 2)
  g_hex <- substr(hash, 7, 8)  # Use different position
  b_hex <- substr(hash, 13, 14)  # Use different position
  
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