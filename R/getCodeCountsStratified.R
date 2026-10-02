#' Get per-stratum code counts for a concept tree
#'
#' @description
#' Retrieves per-concept, per-stratum record counts for the concept tree rooted at
#' `conceptId` (the concept, its descendants, and mapped codes). Complements
#' \code{\link{getConceptRelationships}}, which returns the tree's relationships and
#' concept details.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get stratified counts for
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @return A tibble of per-concept, per-stratum counts for every node in the tree, with columns
#'   `concept_id`, `visit_group_concept_id`, `calendar_year`, `gender_concept_id`, `age_decile`,
#'   `node_record_counts` (events of this concept in the stratum), and
#'   `node_descendant_record_counts` (events of this concept and its descendants in the stratum).
#'
#' @importFrom checkmate assertClass assertIntegerish assertString
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble tibble
#'
#' @export
#'
getCodeCountsStratified <- function(
    CDMdbHandler,
    conceptId,
    codeCountsTable = "code_counts") {
    ParallelLogger::logInfo("getCodeCountsStratified: Getting stratified code counts for conceptId: ", conceptId)
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    conceptId |> checkmate::assertIntegerish(lower = 1)
    codeCountsTable |> checkmate::assertString()

    stratifiedCodeCountsTable <- paste0("stratified_", codeCountsTable)

    connection <- CDMdbHandler$connectionHandler$getConnection()
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    conceptTree <- getConceptTree_memoise(CDMdbHandler, conceptId = conceptId, codeCountsTable = codeCountsTable)
    familyTree <- conceptTree$family_tree
    conceptIdsToGetCounts <- conceptTree$concept_ids

    # - Get counts for the tree's concepts and their mapped source concepts.
    sql <- "
        SELECT concept_id, maps_to_concept_id, visit_group_concept_id, calendar_year, gender_concept_id, age_decile, record_counts
        FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable
        WHERE concept_id IN (@conceptIds) OR maps_to_concept_id IN (@conceptIds);
    "
    codeCounts <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        conceptIds = paste(conceptIdsToGetCounts, collapse = ","),
        stratifiedCodeCountsTable = stratifiedCodeCountsTable
    ) |>
        tibble::as_tibble()

    # Standard-concept aggregation across maps_to_concept_id.
    codeCountsStandard <- codeCounts |>
        dplyr::select(-maps_to_concept_id) |>
        dplyr::group_by(concept_id, visit_group_concept_id, calendar_year, gender_concept_id, age_decile) |>
        dplyr::summarise(
            record_counts = sum(record_counts),
            .groups = "drop"
        )

    # - Get counts table only for descendats and mapped from.
    #   Standard rows go first so concept-maps-to-itself duplicates resolve to the standard row.
    codeCountsPerId <- dplyr::bind_rows(
        codeCountsStandard,
        # source concepts, remove duplicates
        codeCounts |>
            dplyr::select(-concept_id) |>
            dplyr::rename(concept_id = maps_to_concept_id) |>
            dplyr::distinct(concept_id, visit_group_concept_id, calendar_year, gender_concept_id, age_decile, record_counts)
    )  |>
    # If concept maps to itself, bcs concept in concept and source concept columns, dont take it
    dplyr::distinct(concept_id, visit_group_concept_id, calendar_year, gender_concept_id, age_decile, record_counts, .keep_all = TRUE)

    # Plain tree edges only (mapping edges never appear here, only "-1"/"0" self-reference rows do).
    familyTreeDescendants <- familyTree |>
        dplyr::filter(!levels %in% c("-1", "0")) |>
        dplyr::select(parent_concept_id, child_concept_id)

    ancestorTableOfDescendant <- tibble::tibble(
        concept_id = unique(c(conceptId, familyTreeDescendants$child_concept_id))
    ) |>
        dplyr::mutate(
            ancestorTable = purrr::map(
                .x = concept_id,
                .f = ~ {
                    .familyTreeToAncestorTable(familyTreeDescendants, .x) |> dplyr::select(descendant_concept_id)
                }
            )
        ) |>
        tidyr::unnest(ancestorTable)

    nodeDescendantRecordCounts <- ancestorTableOfDescendant |>
        dplyr::inner_join(codeCountsPerId, by = c("descendant_concept_id" = "concept_id")) |>
        dplyr::group_by(concept_id, visit_group_concept_id, calendar_year, gender_concept_id, age_decile) |>
        dplyr::summarise(
            descendant_record_counts = sum(record_counts),
            .groups = "drop"
        )

    stratifiedCodeCounts <- codeCountsPerId |>
        dplyr::full_join(nodeDescendantRecordCounts, by = c("concept_id", "visit_group_concept_id", "calendar_year", "gender_concept_id", "age_decile")) |>
        dplyr::mutate(
            descendant_record_counts = dplyr::if_else(is.na(descendant_record_counts), record_counts, descendant_record_counts),
            record_counts = dplyr::if_else(is.na(record_counts), 0, record_counts)
        ) |>
        dplyr::rename(node_record_counts = record_counts, node_descendant_record_counts = descendant_record_counts)

    return(stratifiedCodeCounts)
}

#' Memoised version of getCodeCountsStratified
#'
#' @description
#' A memoised version of the getCodeCountsStratified function that caches results to improve
#' performance for repeated calls with the same parameters. The CDMdbHandler argument is
#' omitted from the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get stratified counts for
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @importFrom memoise memoise
#'
#' @return Same shape as \code{\link{getCodeCountsStratified}}.
#'
#' @export
getCodeCountsStratified_memoise <- memoise::memoise(
    getCodeCountsStratified,
    omit_args = "CDMdbHandler"
)
