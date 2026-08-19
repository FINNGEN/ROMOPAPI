#' Get person-count breakdowns and set overlaps for a concept tree
#'
#' @description
#' Computes exact distinct-person breakdowns for the concept tree rooted at
#' `conceptId`, reading the `stratified_persons` person-level bridge table (see
#' `createStratifiedPersonsTable()`). Per-concept `person_counts` and
#' `descendant_person_counts` already live in `code_counts` and are reachable
#' via `getCodeCounts()`'s `concepts` tibble — this function covers what that
#' table can't answer: stratum breakdowns and exact set-overlap (UpSet) regions.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get person counts for
#' @param level Maximum tree depth (distance from `conceptId`) of concepts to include
#'   in `upset_person_counts`. NULL (default) includes the full tree.
#' @param sexStratum Optional integer vector of `gender_concept_id` values to restrict
#'   `upset_person_counts` to. NULL or empty (default) includes all.
#' @param ageStratum Optional integer vector of `age_decile` values to restrict
#'   `upset_person_counts` to. NULL or empty (default) includes all.
#' @param yearStratum Optional integer vector of `calendar_year` values to restrict
#'   `upset_person_counts` to. NULL or empty (default) includes all.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values to
#'   restrict `upset_person_counts` to. NULL or empty (default) includes all.
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @return A list containing:
#' \itemize{
#'   \item `filter_person_counts` - Tibble of `filter` ("sex"/"age"/"visit"), `stratum`,
#'     `person_counts` — a breakdown of the full tree's persons by each filterable
#'     dimension, independent of `level` and the stratum arguments. Lets a client
#'     discover which strata are worth filtering by, and how many persons they hold.
#'   \item `upset_person_counts` - Tibble of `group` (the concept IDs of the exclusive
#'     region, joined by "-") and `person_counts` — exact UpSet exclusive-region counts
#'     for the concepts at or under `level`, restricted to the requested strata.
#' }
#'
#' @importFrom checkmate assertClass assertIntegerish assertString
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter distinct pull mutate group_by summarise count if_else
#' @importFrom stringr str_detect str_extract
#'
#' @export
getPersonCounts <- function(
    CDMdbHandler,
    conceptId,
    level = NULL,
    sexStratum = NULL,
    ageStratum = NULL,
    yearStratum = NULL,
    visitStratum = NULL,
    codeCountsTable = "code_counts") {
    ParallelLogger::logInfo("getPersonCounts: Getting person counts for conceptId: ", conceptId)
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    conceptId |> checkmate::assertIntegerish(lower = 1)
    level |> checkmate::assertIntegerish(lower = 0, null.ok = TRUE)
    sexStratum |> checkmate::assertIntegerish(null.ok = TRUE)
    ageStratum |> checkmate::assertIntegerish(null.ok = TRUE)
    yearStratum |> checkmate::assertIntegerish(null.ok = TRUE)
    visitStratum |> checkmate::assertIntegerish(null.ok = TRUE)
    codeCountsTable |> checkmate::assertString()

    stratifiedPersonsTable <- "stratified_persons"

    connection <- CDMdbHandler$connectionHandler$getConnection()
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    # - Get the concept tree (shared with getCodeCounts, memoised)
    conceptTree <- getConceptTree_memoise(CDMdbHandler, conceptId = conceptId, codeCountsTable = codeCountsTable)
    familyTree <- conceptTree$family_tree
    treeConceptIds <- conceptTree$concept_ids

    # - Depth (distance from conceptId) of every node, to apply `level`
    nodeLevels <- familyTree |>
        dplyr::filter(child_concept_id %in% treeConceptIds) |>
        dplyr::distinct(child_concept_id, levels) |>
        dplyr::mutate(
            min_level = dplyr::if_else(
                stringr::str_detect(levels, "^\\d+-\\d+$"),
                as.integer(stringr::str_extract(levels, "^\\d+")),
                0L
            )
        )

    overLevelConceptIds <- if (is.null(level)) {
        treeConceptIds
    } else {
        nodeLevels |>
            dplyr::filter(min_level <= level) |>
            dplyr::pull(child_concept_id) |>
            unique()
    }

    # - filter_person_counts: breakdown of the FULL tree's persons by sex/age/visit,
    #   independent of `level` and the stratum arguments — lets a client discover
    #   which strata are worth filtering by.
    sql <- "
        WITH tree_persons AS (
            SELECT DISTINCT person_id, gender_concept_id, age_decile, visit_group_concept_id
            FROM @resultsDatabaseSchema.@stratifiedPersonsTable
            WHERE concept_id IN (@conceptIds) OR maps_to_concept_id IN (@conceptIds)
        )
        SELECT 'sex' AS filter, CAST(gender_concept_id AS BIGINT) AS stratum, COUNT(DISTINCT person_id) AS person_counts
        FROM tree_persons GROUP BY gender_concept_id
        UNION ALL
        SELECT 'age' AS filter, CAST(age_decile AS BIGINT) AS stratum, COUNT(DISTINCT person_id) AS person_counts
        FROM tree_persons GROUP BY age_decile
        UNION ALL
        SELECT 'visit' AS filter, CAST(visit_group_concept_id AS BIGINT) AS stratum, COUNT(DISTINCT person_id) AS person_counts
        FROM tree_persons GROUP BY visit_group_concept_id;
    "
    filterPersonCounts <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedPersonsTable = stratifiedPersonsTable,
        conceptIds = paste(treeConceptIds, collapse = ",")
    ) |>
        tibble::as_tibble()
    names(filterPersonCounts) <- tolower(names(filterPersonCounts))

    # - upset_person_counts: exact set-overlap regions for the concepts at/under
    #   `level`, restricted to the requested strata. Pull the (person, target
    #   concept) membership pairs and build the exclusive-region key in R — the
    #   membership pull is bounded by the (small, user-picked) concept set.
    strataFilterSql <- paste0(
        .inFilterSql("gender_concept_id", sexStratum),
        .inFilterSql("age_decile", ageStratum),
        .inFilterSql("calendar_year", yearStratum),
        .inFilterSql("visit_group_concept_id", visitStratum)
    )

    sql <- paste0(
        "SELECT DISTINCT person_id, concept_id AS target_concept_id
         FROM @resultsDatabaseSchema.@stratifiedPersonsTable
         WHERE concept_id IN (@targetConceptIds)", strataFilterSql, "
         UNION
         SELECT DISTINCT person_id, maps_to_concept_id AS target_concept_id
         FROM @resultsDatabaseSchema.@stratifiedPersonsTable
         WHERE maps_to_concept_id IN (@targetConceptIds)", strataFilterSql, ";"
    )
    membership <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedPersonsTable = stratifiedPersonsTable,
        targetConceptIds = paste(overLevelConceptIds, collapse = ",")
    ) |>
        tibble::as_tibble()
    names(membership) <- tolower(names(membership))

    upsetPersonCounts <- membership |>
        dplyr::mutate(target_concept_id = as.integer(target_concept_id)) |>
        dplyr::group_by(person_id) |>
        dplyr::summarise(
            group = paste(sort(unique(target_concept_id)), collapse = "-"),
            .groups = "drop"
        ) |>
        dplyr::count(group, name = "person_counts")

    return(list(
        filter_person_counts = filterPersonCounts,
        upset_person_counts = upsetPersonCounts
    ))
}

# Builds " AND @column IN (v1,v2,...)", or "" when values is NULL/empty.
# values are always integer ids validated by checkmate upstream — safe to splice.
.inFilterSql <- function(column, values) {
    if (is.null(values) || length(values) == 0) {
        return("")
    }
    paste0(" AND ", column, " IN (", paste(as.integer(values), collapse = ","), ")")
}

#' Memoised version of getPersonCounts
#'
#' @description
#' A memoised version of the getPersonCounts function that caches results to improve
#' performance for repeated calls with the same parameters. The CDMdbHandler argument is
#' omitted from the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get person counts for
#' @param level Maximum tree depth to include in `upset_person_counts`. NULL includes the full tree.
#' @param sexStratum Optional integer vector of `gender_concept_id` values to restrict to.
#' @param ageStratum Optional integer vector of `age_decile` values to restrict to.
#' @param yearStratum Optional integer vector of `calendar_year` values to restrict to.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values to restrict to.
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @importFrom memoise memoise
#'
#' @return Same shape as \code{\link{getPersonCounts}}.
#'
#' @export
getPersonCounts_memoise <- memoise::memoise(
    getPersonCounts,
    omit_args = "CDMdbHandler"
)
