#' Get code counts and related concept information
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Vector of concept IDs to get counts and relationships for
#'
#' @return A list containing:
#' \itemize{
#'   \item concept_relationships - Tibble of concept relationships (Maps to, Subsumes)
#'   \item concepts - Tibble of concept details for related concepts
#'   \item code_counts - Tibble of code counts from the code_counts table
#' }
#'
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom tibble as_tibble
#' @importFrom dplyr pull
#'
#' @export
getCodeCounts <- function(
    CDMdbHandler,
    conceptIds) {
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    conceptIds |> checkmate::assertIntegerish(min.len = 1)


    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema


    #
    # FUNCTION
    #

    # - Get concept_relationships
    # Get all the relationships for the conceptIds of type 'Maps to', 'Mapped from', 'Is a', 'Subsumes'
    sql <- "SELECT * FROM @vocabularyDatabaseSchema.concept_relationship WHERE relationship_id IN ('Maps to','Mapped from', 'Is a', 'Subsumes') AND concept_id_1 IN (@conceptIds);"
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, conceptIds = paste(conceptIds, collapse = ","))
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    concept_relationships <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble() |>
        dplyr::select(
            concept_id_1,
            concept_id_2,
            relationship_id
        ) |> 
        dplyr::filter(
            concept_id_1 != concept_id_2
        )

    descendantConceptIds <- c(
        concept_relationships |>
            dplyr::pull(concept_id_2),
        concept_relationships |>
            dplyr::pull(concept_id_1)
    ) |>
        unique()

    # - Get code counts
    # Get all the code counts for the descendantConceptIds
    sql <- "SELECT * FROM @resultsDatabaseSchema.code_counts WHERE concept_id IN (@descendantConceptIds);"
    sql <- SqlRender::render(sql, resultsDatabaseSchema = resultsDatabaseSchema, descendantConceptIds = paste(descendantConceptIds, collapse = ","))
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    code_counts <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble() |>
        dplyr::select(-domain)

    # if the code_count is 0 for 'Mapped from' or 'Mapped to', remove from concept_relationships
    concept_relationships <- concept_relationships |>
        dplyr::semi_join(
            code_counts,
            by = c( "concept_id_2" = "concept_id")
        )

    descendantConceptIds <- c(
        concept_relationships |>
            dplyr::pull(concept_id_2),
        concept_relationships |>
            dplyr::pull(concept_id_1)
    ) |>
        unique()

    # - Get concepts
    # Get all the concepts for the descendantConceptIds
    sql <- "SELECT * FROM @vocabularyDatabaseSchema.concept WHERE concept_id IN (@descendantConceptIds);"
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema, descendantConceptIds = paste(descendantConceptIds, collapse = ","))
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    concepts <- DatabaseConnector::dbGetQuery(connection, sql) |> tibble::as_tibble()



    return(list(
        concept_relationships = concept_relationships,
        concepts = concepts,
        code_counts = code_counts
    ))
}


#' Get the CDM source information
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#'
#' @return A tibble of the CDM source information
#'
#' @export
getCDMSource <- function(
    CDMdbHandler) {
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
    sql <- SqlRender::render(sql, vocabularyDatabaseSchema = vocabularyDatabaseSchema)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    cdm_source <- DatabaseConnector::dbGetQuery(connection, sql) |> tibble::as_tibble()

    return(cdm_source)
}
