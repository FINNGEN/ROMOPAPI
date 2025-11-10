#' Get the API information
#'
#' @description
#' Retrieves the CDM source information from the vocabulary schema of an OMOP CDM database.
#' Retrieves the API information from the vocabulary schema of an OMOP CDM database.
#' This includes metadata about the API, version, and other administrative details.
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#'
#' @return A tibble containing the CDM source information with columns from the cdm_source table
#'
#' @importFrom checkmate assertClass
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#'
#' @export
#'
getAPIInfo <- function(
    CDMdbHandler) {
    ParallelLogger::logInfo("getAPIInfo: Getting API information")
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")

    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    sql <- "SELECT * FROM @vocabularyDatabaseSchema.cdm_source;"
    cdm_source <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
    ) |> tibble::as_tibble()

    info <- list(
        cdm_source_name = cdm_source$cdm_source_name,
        cdm_source_abbreviation = cdm_source$cdm_source_abbreviation,
        vocabulary_version = cdm_source$vocabulary_version,
        romop_api_version = as.character(utils::packageVersion("ROMOPAPI"))
    )

    return(info)
}
