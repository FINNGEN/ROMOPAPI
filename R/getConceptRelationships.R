#' Get concept relationships and concept details for a concept
#'
#' @description
#' Retrieves the family tree (ancestors, descendants and mappings) for a concept, along
#' with concept metadata and record/person counts for every concept referenced in that
#' tree. Complements \code{\link{getCodeCountsStratified}}, which returns the per-stratum
#' counts for the same tree.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get relationships for
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @return A list containing:
#' \itemize{
#'   \item `concept_relationships` - Tibble of concept relationships including 'Maps to', 'Mapped from', 'Parent', and descendant relationships
#'   \item `concepts` - Tibble of concept details for related concepts
#' }
#'
#' @importFrom checkmate assertClass assertIntegerish assertString
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble tibble
#'
#' @export
#'
getConceptRelationships <- function(
    CDMdbHandler,
    conceptId,
    codeCountsTable = "code_counts") {
    ParallelLogger::logInfo("getConceptRelationships: Getting concept relationships for conceptId: ", conceptId)
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    conceptId |> checkmate::assertIntegerish(lower = 1)
    codeCountsTable |> checkmate::assertString()

    stratifiedCodeCountsTable <- paste0("stratified_", codeCountsTable)

    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    conceptTree <- getConceptTree_memoise(CDMdbHandler, conceptId = conceptId, codeCountsTable = codeCountsTable)
    familyTreeWithInfo <- conceptTree$family_tree
    conceptIdsToGetCounts <- conceptTree$concept_ids

    # - Get the distinct (concept_id, maps_to_concept_id) pairs for the tree, to derive
    #   'Maps to' and 'Mapped from' edges.
    sql <- "
        SELECT DISTINCT concept_id, maps_to_concept_id
        FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable
        WHERE concept_id IN (@conceptIds) OR maps_to_concept_id IN (@conceptIds);
    "
    mappingPairs <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        conceptIds = paste(conceptIdsToGetCounts, collapse = ","),
        stratifiedCodeCountsTable = stratifiedCodeCountsTable
    ) |>
        tibble::as_tibble()

    # - Derive 'Maps to' and 'Mapped from'
    mappings <- dplyr::bind_rows(
        mappingPairs |>
            dplyr::mutate(levels = "Mapped from"),
        mappingPairs |>
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

    # - Get concept details + counts, scoped to every concept referenced in the tree
    #   (including mapped-to concepts added above).
    treeConceptIds <- familyTreeWithMappings |>
        dplyr::distinct(child_concept_id) |>
        dplyr::pull(child_concept_id)

    sql <- "
        SELECT DISTINCT
            c.concept_id,
            c.concept_name, c.domain_id, c.vocabulary_id, c.concept_class_id, c.standard_concept, c.concept_code,
            cc.record_counts, cc.descendant_record_counts, cc.person_counts, cc.descendant_person_counts
        FROM @vocabularyDatabaseSchema.concept c
        INNER JOIN @resultsDatabaseSchema.@codeCountsTable cc
        ON c.concept_id = cc.concept_id
        WHERE c.concept_id IN (@conceptIds);
    "
    conceptsWithCodeCounts <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        resultsDatabaseSchema = resultsDatabaseSchema,
        codeCountsTable = codeCountsTable,
        conceptIds = paste(treeConceptIds, collapse = ",")
    ) |>
        tibble::as_tibble() |>
        dplyr::mutate(standard_concept = dplyr::if_else(is.na(standard_concept), FALSE, TRUE)) |>
        dplyr::mutate(concept_id = as.double(concept_id))

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
        concepts = concepts
    ))
}

#' Memoised version of getConceptRelationships
#'
#' @description
#' A memoised version of the getConceptRelationships function that caches results to improve
#' performance for repeated calls with the same parameters. The CDMdbHandler argument is
#' omitted from the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get relationships for
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @importFrom memoise memoise
#'
#' @return Same shape as \code{\link{getConceptRelationships}}.
#'
#' @export
getConceptRelationships_memoise <- memoise::memoise(
    getConceptRelationships,
    omit_args = "CDMdbHandler"
)
