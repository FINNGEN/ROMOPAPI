createReport <- function(conceptId, CDMdbHandler, showsMappings = FALSE, pruneLevels = NULL) {
    conceptId |> checkmate::assertIntegerish()
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    showsMappings |> checkmate::assertLogical(null.ok = TRUE)
    pruneLevels |> checkmate::assertIntegerish(null.ok = TRUE)

    # Render Rmd to a temporary HTML file
    tmp_html <- tempfile(fileext = ".html")
    rmarkdown::render(system.file("reports", "testReport.Rmd", package = "ROMOPAPI"),
        output_file = tmp_html,
        quiet = TRUE,
        params = list(
            conceptId = conceptId,
            CDMdbHandler = CDMdbHandler,
            showsMappings = showsMappings,
            pruneLevels = pruneLevels
        )
    )

    return(tmp_html)
}
