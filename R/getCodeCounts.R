#' Get code counts and related concept information
#'
#' @description
#' Retrieves code counts and concept relationships for specified concept IDs from an OMOP CDM database.
#' This function fetches parent concepts, descendant concepts, and mapped concepts, along with
#' their associated event counts.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get counts and relationships for
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @return A list containing:
#' \itemize{
#'   \item `concept_relationships` - Tibble of concept relationships including 'Maps to', 'Mapped from', 'Parent', and descendant relationships
#'   \item `concepts` - Tibble of concept details for related concepts
#'   \item `code_counts` - Tibble of code counts from the code_counts table
#' }
#'
#' @importFrom checkmate assertClass assertIntegerish assertString
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get code counts for specific concept IDs
#' result <- getCodeCounts(CDMdbHandler, conceptId = 317009)
#'
#' # View concept relationships
#' print(result$concept_relationships)
#' }
getCodeCounts <- function(
    CDMdbHandler,
    conceptId,
    codeCountsTable = "code_counts") {
    ParallelLogger::logInfo("getCodeCounts: Getting code counts for conceptId: ", conceptId)
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
    familyTreeWithInfo <- conceptTree$family_tree
    conceptIdsToGetCounts <- conceptTree$concept_ids

    # - Get counts and derive 'Maps to' and 'Mapped from' from that.
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

    # - Derive 'Maps to' and 'Mapped from'
    mappings <- dplyr::bind_rows(
        codeCounts |>
            dplyr::distinct(concept_id, maps_to_concept_id) |>
            dplyr::mutate(levels = "Mapped from"),
        codeCounts |>
            dplyr::distinct(concept_id, maps_to_concept_id) |>
            dplyr::rename(maps_to_concept_id = concept_id, concept_id = maps_to_concept_id) |>
            dplyr::mutate(levels = "Maps to")
    ) |> 
    # If concept maps to itself, bcs concept in concept and source concept columns, dont take it
    dplyr::filter(concept_id != maps_to_concept_id)

    familyTreeWithMappings <- dplyr::bind_rows(
        familyTreeWithInfo,
        familyTreeWithInfo |>
            dplyr::distinct(child_concept_id) |>
            dplyr::inner_join(mappings, by = c("child_concept_id" = "concept_id")) |>
            dplyr::rename(parent_concept_id = child_concept_id, child_concept_id = maps_to_concept_id)
    )

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

    familyTreeDescendants <- familyTreeWithMappings |>
        dplyr::filter(!levels %in% c("Mapped from", "Maps to", "-1", "0")) |>
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

    # - Get concept details
    conceptsWithCodeCounts <- getConceptsWithCodeCounts_memoise(CDMdbHandler, codeCountsTable = codeCountsTable) |> 
        dplyr::select(-number_of_descendants)
    concepts <- familyTreeWithMappings |>
        dplyr::distinct(child_concept_id) |>
        dplyr::rename(concept_id = child_concept_id) |>
        dplyr::left_join(conceptsWithCodeCounts, by = c("concept_id" = "concept_id"))


    # TEMP: in eunomia missing concepts
    missingConcepts <- concepts |>
        dplyr::mutate(
            concept_name = dplyr::if_else(is.na(concept_name), "Missing concept Name", concept_name),
            domain_id = dplyr::if_else(is.na(domain_id), "NA", domain_id),
            vocabulary_id = dplyr::if_else(is.na(vocabulary_id), "NA", vocabulary_id),
            concept_class_id = dplyr::if_else(is.na(concept_class_id), "NA", concept_class_id),
            standard_concept = dplyr::if_else(is.na(standard_concept), TRUE, standard_concept),
            concept_code = dplyr::if_else(is.na(concept_code), "NA", concept_code),
            record_counts = dplyr::if_else(is.na(record_counts), 0, record_counts),
            descendant_record_counts = dplyr::if_else(is.na(descendant_record_counts), 0, descendant_record_counts)
        )
    # END TEMP

    # add concept_class_id to familyTreeWithRelationships
    conceptRelationships <- familyTreeWithMappings |>
        dplyr::left_join(conceptsWithCodeCounts |> dplyr::select(concept_id, concept_class_id), by = c("child_concept_id" = "concept_id")) |>
        dplyr::select(-paths)

    return(list(
        concept_relationships = conceptRelationships,
        stratified_code_counts = stratifiedCodeCounts,
        concepts = concepts
    ))
}

#' Memoised version of getCodeCounts
#'
#' @description
#' A memoised version of the getCodeCounts function that caches results to improve performance
#' for repeated calls with the same parameters. The CDMdbHandler argument is omitted from
#' the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get counts and relationships for
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @importFrom memoise memoise
#' 
#' @return A list containing:
#' \itemize{
#'   \item `concept_relationships` - Tibble of concept relationships including 'Maps to', 'Mapped from', 'Parent', and descendant relationships
#'   \item `concepts` - Tibble of concept details for related concepts
#'   \item `code_counts` - Tibble of code counts from the code_counts table
#' }
#'
#' @export
getCodeCounts_memoise <- memoise::memoise(
    getCodeCounts,
    omit_args = "CDMdbHandler"
)