#' Create a dynamic HTML report for concept analysis
#'
#' @description
#' Generates a comprehensive HTML report for analyzing OMOP concept hierarchies and code counts.
#' The report includes interactive visualizations, tables, and graphs showing concept relationships,
#' stratified counts, and mappings based on the provided parameters.
#'
#' @param conceptId An integer or vector of integers representing OMOP concept IDs to analyze.
#'   The report will focus on these concepts and their hierarchical relationships.
#' @param CDMdbHandler A CDMdbHandler object containing database connection details and schema information.
#' @param showsMappings Logical indicating whether to display concept mappings in visualizations.
#'   Defaults to FALSE. When TRUE, additional mapping information is included in graphs and tables.
#' @param pruneLevels Integer vector specifying which hierarchical levels to include in the analysis.
#'   NULL includes all levels. Values represent the depth from the root concept (0 = root, 1 = first level, etc.).
#' @param pruneClass Character string specifying a concept class to filter results.
#'   NULL includes all concept classes. Common values include "Ingredient", "Clinical Drug", etc.
#'
#' @return A character string containing the file path to the generated HTML report.
#'   The file is created as a temporary file and should be cleaned up by the caller.
#'
#' @details
#' This function creates a comprehensive report by:
#' \itemize{
#'   \item Retrieving code counts and concept relationships using \code{getCodeCounts_memoise}
#'   \item Optionally pruning results based on hierarchical levels and concept classes
#'   \item Generating interactive Mermaid diagrams showing concept hierarchies
#'   \item Creating detailed tables with stratified code counts
#'   \item Producing visualizations with concept relationships and mappings
#' }
#'
#' The report template includes:
#' \itemize{
#'   \item Dynamic title showing concept ID and parameters
#'   \item Interactive Mermaid graph visualization
#'   \item Comprehensive data tables
#'   \item Statistical plots and charts
#' }
#'
#' @importFrom checkmate assertIntegerish assertClass assertLogical assertCharacter
#' @importFrom DiagrammeR mermaid
#' 
#' 
#' @export
#' 
createReport <- function(conceptId, CDMdbHandler, showsMappings = FALSE, pruneLevels = NULL, pruneClass = NULL) {
    conceptId |> checkmate::assertIntegerish()
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    showsMappings |> checkmate::assertLogical(null.ok = TRUE)
    pruneLevels |> checkmate::assertIntegerish(null.ok = TRUE)
    pruneClass |> checkmate::assertCharacter(null.ok = TRUE)

    # Render Rmd to a temporary HTML file
    tmp_html <- tempfile(fileext = ".html")
    rmarkdown::render(system.file("reports", "testReport.Rmd", package = "ROMOPAPI"),
        output_file = tmp_html,
        quiet = TRUE,
        params = list(
            conceptId = conceptId,
            CDMdbHandler = CDMdbHandler,
            showsMappings = showsMappings,
            pruneLevels = pruneLevels,
            pruneClass = pruneClass
        )
    )

    return(tmp_html)
}
