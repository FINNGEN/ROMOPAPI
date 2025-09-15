#' Get code counts and related concept information
#'
#' @description
#' Retrieves code counts and concept relationships for specified concept IDs from an OMOP CDM database.
#' This function fetches parent concepts, descendant concepts, and mapped concepts, along with
#' their associated event counts.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get counts and relationships for
#'
#' @return A list containing:
#' \itemize{
#'   \item `concept_relationships` - Tibble of concept relationships including 'Maps to', 'Mapped from', 'Parent', and descendant relationships
#'   \item `concepts` - Tibble of concept details for related concepts
#'   \item `code_counts` - Tibble of code counts from the code_counts table
#' }
#'
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom SqlRender render translate
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom tibble as_tibble
#' @importFrom dplyr pull bind_rows
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
    message("getCodeCounts: ", conceptId)
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

    # - Get concept parents and all descendants, only if they have code counts
    sql <- "
    -- All descendants from the concept id
    WITH concept_descendants AS (
        SELECT DISTINCT
            ca.ancestor_concept_id AS concept_id,
            ca.descendant_concept_id AS descendant_concept_id
        FROM @vocabularyDatabaseSchema.concept_ancestor ca
        WHERE ca.ancestor_concept_id IN (@conceptId) AND (ca.min_levels_of_separation != 0 OR ca.ancestor_concept_id = ca.descendant_concept_id)
    ),
    -- Only  descendants from the concept id that have counts
    concept_descendants_with_counts AS (
        SELECT
            cd.descendant_concept_id AS concept_id_with_counts
        FROM
            concept_descendants AS cd
        INNER JOIN (
            SELECT DISTINCT
                    concept_id AS concept_id
            FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable
            UNION ALL
            SELECT DISTINCT
                    maps_to_concept_id AS concept_id
            FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable
        ) AS cca
        ON cd.descendant_concept_id = cca.concept_id
    ),
    -- Descendants from the concept id that have code or children with code
    concept_descendants_with_counts_or_descendance_counts AS (
        SELECT
            -- cd.concept_id AS concept_id
            cd.descendant_concept_id AS descendant_concept_id
        FROM concept_descendants AS cd
        INNER JOIN @vocabularyDatabaseSchema.concept_ancestor ca
        ON cd.descendant_concept_id = ca.ancestor_concept_id
        INNER JOIN concept_descendants_with_counts AS cdc
        ON ca.descendant_concept_id = cdc.concept_id_with_counts
        WHERE ca.min_levels_of_separation != 0 OR ca.ancestor_concept_id = ca.descendant_concept_id
    ),
    temp_tree AS (
    -- From all the descendant nodes with record counts or descendant record counts, take the parents
     SELECT DISTINCT
            ca.ancestor_concept_id as parent_concept_id,
            cddrc.descendant_concept_id as child_concept_id
        FROM concept_descendants_with_counts_or_descendance_counts AS cddrc
    -- Append the parents to each descendant
    LEFT JOIN @vocabularyDatabaseSchema.concept_ancestor ca
    ON cddrc.descendant_concept_id = ca.descendant_concept_id
    WHERE ca.min_levels_of_separation = 1
    )
    -- take only parents who are someones children, or are the parents  of the concept id
    SELECT * FROM temp_tree tt
    WHERE parent_concept_id IN (SELECT DISTINCT child_concept_id FROM temp_tree ) OR child_concept_id IN (@conceptId)
    "

    sql <- SqlRender::render(
        sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptId = conceptId,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedCodeCountsTable = stratifiedCodeCountsTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)

    # Gets tree of descendants and the code counts for each descendant
    familyTree <- DatabaseConnector::dbGetQuery(connection, sql) |>
        tibble::as_tibble()

    ancestorTable <- .familyTreeToAncestorTable(familyTree, conceptId)

    familyTreeWithInfo <- familyTree |>
        dplyr::left_join(ancestorTable, by = c("child_concept_id" = "descendant_concept_id"))

    familyTreeWithInfo <- dplyr::bind_rows(
        familyTreeWithInfo |> dplyr::filter(levels != "0-0"),
        tibble::tibble(
            parent_concept_id = familyTreeWithInfo |> dplyr::filter(levels == "0-0") |> pull(child_concept_id),
            child_concept_id = familyTreeWithInfo |> dplyr::filter(levels == "0-0") |> pull(parent_concept_id),
            levels = "-1",
            paths = 1
        ),
        tibble::tibble(
            parent_concept_id = conceptId,
            child_concept_id = conceptId,
            levels = "0",
            paths = 0
        )
    ) |>
        dplyr::arrange(levels)


    conceptIdsToGetCounts <- familyTreeWithInfo |>
        dplyr::filter(!levels %in% c("-1")) |>
        dplyr::pull(child_concept_id) |>
        unique()


    # - Get counts and derive 'Maps to' and 'Mapped from' from that
    sql <- "SELECT * FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable WHERE concept_id IN (@conceptIds) OR maps_to_concept_id IN (@conceptIds);"
    sql <- SqlRender::render(
        sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        conceptIds = paste(conceptIdsToGetCounts, collapse = ","),
        stratifiedCodeCountsTable = stratifiedCodeCountsTable
    )
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
    codeCounts <- DatabaseConnector::dbGetQuery(connection, sql) |>
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
    )

    familyTreeWithMappings <- dplyr::bind_rows(
        familyTreeWithInfo,
        familyTreeWithInfo |>
            dplyr::distinct(child_concept_id) |>
            dplyr::inner_join(mappings, by = c("child_concept_id" = "concept_id")) |>
            dplyr::rename(parent_concept_id = child_concept_id, child_concept_id = maps_to_concept_id)
    )

    # - Get counts table only for descendats and mapped from
    codeCountsPerId <- dplyr::bind_rows(
        # source concepts, remove duplicates
        codeCounts |> 
            dplyr::select(-concept_id) |> 
            dplyr::rename(concept_id = maps_to_concept_id) |> 
            dplyr::distinct(concept_id, calendar_year, gender_concept_id, age_decile, record_counts),
        # standard concepts, agregate counts
        codeCounts |> dplyr::select(-maps_to_concept_id) |> 
            dplyr::group_by(concept_id, calendar_year, gender_concept_id, age_decile) |>
            dplyr::summarise(record_counts = sum(record_counts), .groups = "drop")
    ) 

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
        dplyr::group_by(concept_id, calendar_year, gender_concept_id, age_decile) |>
        dplyr::summarise(
            descendant_record_counts = sum(record_counts),
            .groups = "drop"
        )

    stratifiedCodeCounts <- codeCountsPerId |>
        dplyr::full_join(nodeDescendantRecordCounts, by = c("concept_id", "calendar_year", "gender_concept_id", "age_decile")) |> 
        dplyr::mutate(
            descendant_record_counts = dplyr::if_else(is.na(descendant_record_counts), record_counts, descendant_record_counts),
            record_counts = dplyr::if_else(is.na(record_counts), 0, record_counts)
        ) |>
        dplyr::rename(node_record_counts = record_counts, node_descendant_record_counts = descendant_record_counts)

    # - Get concept details
    conceptsWithCodeCounts <- getConceptsWithCodeCounts_memoise(CDMdbHandler, codeCountsTable = codeCountsTable)
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

#' Convert family tree to ancestor table
#'
#' @description
#' Takes a family tree structure and a concept ID and generates an ancestor table showing
#' descendant relationships and path information.
#'
#' @param familyTree A tibble containing parent-child concept relationships
#' @param conceptId The concept ID to generate the ancestor table for
#'
#' @return A tibble containing:
#' \itemize{
#'   \item descendant_concept_id - The descendant concept IDs
#'   \item levels - String showing min and max levels of the relationship
#'   \item paths - Number of paths to reach the descendant
#' }
#'
#' @importFrom tibble tibble
#' @importFrom dplyr semi_join filter mutate select bind_rows group_by summarise arrange n
#'
.familyTreeToAncestorTable <- function(familyTree, conceptId) {
    descendantTable <- tibble::tibble(
        descendant_concept_id = conceptId,
        level = 0,
        paths = 0
    )

    level <- 0
    while (TRUE) {
        a <- familyTree |> dplyr::semi_join(
            descendantTable |>
                dplyr::filter(level == {{ level }}),
            by = c("parent_concept_id" = "descendant_concept_id")
        )

        if (nrow(a) == 0) {
            break
        }

        level <- level + 1

        descendantTable <- dplyr::bind_rows(
            descendantTable,
            a |>
                dplyr::select(descendant_concept_id = child_concept_id) |>
                dplyr::mutate(level = {{ level }})
        )
    }

    descendantTable <- descendantTable |>
        dplyr::group_by(descendant_concept_id) |>
        dplyr::summarise(
            level = min(level),
            levels = paste0(min(level), "-", max(level)),
            paths = dplyr::n(),
            .groups = "drop"
        ) |>
        dplyr::arrange(level) |>
        dplyr::select(descendant_concept_id, levels, paths)

    return(descendantTable)
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
