#' Get the concept relationship tree for a concept
#'
#' @description
#' Builds the family tree (ancestors and descendants) for a concept, pruned to
#' only the nodes that have code counts or have a descendant with code counts
#' — the same "counted, or has a counted descendant" rule `code_counts` uses to
#' decide which concepts get a row. Shared by `getConceptRelationships()`,
#' `getCodeCountsStratified()`, `getPersonCountsFilters()` and
#' `getPersonCountsUpset()` so the tree is built (and memoised) once.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get the tree for
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @return A list containing:
#' \itemize{
#'   \item `family_tree` - Tibble of `parent_concept_id`, `child_concept_id`, `levels`, `paths` edges
#'   \item `concept_ids` - Unique vector of concept IDs in the tree (excludes the reverse-parent `"-1"` edge)
#' }
#'
#' @importFrom checkmate assertClass assertIntegerish assertString
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr pull bind_rows left_join filter arrange
#'
#' @export
getConceptTree <- function(
    CDMdbHandler,
    conceptId,
    codeCountsTable = "code_counts") {
    ParallelLogger::logInfo("getConceptTree: Getting concept tree for conceptId: ", conceptId)
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
    WHERE parent_concept_id IN (SELECT DISTINCT child_concept_id FROM temp_tree ) OR
      child_concept_id IN (@conceptId) OR
      parent_concept_id IN (@conceptId) -- when concept is top in tree
    "

    # Gets tree of descendants and the code counts for each descendant
    familyTree <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptId = conceptId,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedCodeCountsTable = stratifiedCodeCountsTable
    ) |>
    tibble::as_tibble()

    if (nrow(familyTree) == 0) {
        stop("No family tree found for conceptId: ", conceptId)
    }

    ancestorTable <- .familyTreeToAncestorTable(familyTree, conceptId)

    familyTreeWithInfo <- familyTree |>
        dplyr::left_join(ancestorTable, by = c("child_concept_id" = "descendant_concept_id"))

    familyTreeWithInfo <- dplyr::bind_rows(
        familyTreeWithInfo |> dplyr::filter(levels != "0-0"),
        tibble::tibble(
            parent_concept_id = familyTreeWithInfo |> dplyr::filter(levels == "0-0") |> dplyr::pull(child_concept_id),
            child_concept_id = familyTreeWithInfo |> dplyr::filter(levels == "0-0") |> dplyr::pull(parent_concept_id),
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

    conceptIds <- familyTreeWithInfo |>
        dplyr::filter(!levels %in% c("-1")) |>
        dplyr::pull(child_concept_id) |>
        unique()

    return(list(
        family_tree = familyTreeWithInfo,
        concept_ids = conceptIds
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


#' Memoised version of getConceptTree
#'
#' @description
#' A memoised version of the getConceptTree function that caches results to improve performance
#' for repeated calls with the same parameters. The CDMdbHandler argument is omitted from
#' the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get the tree for
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @importFrom memoise memoise
#'
#' @return Same shape as \code{\link{getConceptTree}}.
#'
#' @export
getConceptTree_memoise <- memoise::memoise(
    getConceptTree,
    omit_args = "CDMdbHandler"
)
