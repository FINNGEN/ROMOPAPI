#' Get exact set-overlap (UpSet) person counts for a concept tree
#'
#' @description
#' Computes exact UpSet exclusive-region person counts for the concept tree rooted at
#' `conceptId`, reading the `stratified_persons` person-level bridge table (see
#' `createStratifiedPersonsTable()`). Reuses the memoised tree getter so the tree is only
#' computed once across repeated calls.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get person counts for
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`, restricting
#'   the regions to that inclusive calendar-year range. NULL or empty (default) uses the
#'   full range.
#' @param level Maximum tree depth (distance from `conceptId`) of concepts to include.
#'   NULL (default) includes the full tree.
#' @param sexStratum Optional integer vector of `gender_concept_id` values to restrict to.
#'   NULL or empty (default) includes all.
#' @param ageStratum Optional integer vector of `age_decile` values to restrict to.
#'   NULL or empty (default) includes all.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values to
#'   restrict to. NULL or empty (default) includes all.
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @return A tibble of `group` (the concept IDs of the exclusive region, joined by "-")
#'   and `person_counts` — exact UpSet exclusive-region counts for the concepts at or
#'   under `level`, restricted to `yearsRange` and the requested strata.
#'
#' @importFrom checkmate assertClass assertIntegerish assertString
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter distinct pull mutate group_by summarise count if_else
#' @importFrom stringr str_detect str_extract
#'
#' @export
getPersonCountsUpset <- function(
    CDMdbHandler,
    conceptId,
    yearsRange = NULL,
    level = NULL,
    sexStratum = NULL,
    ageStratum = NULL,
    visitStratum = NULL,
    codeCountsTable = "code_counts") {
    ParallelLogger::logInfo("getPersonCountsUpset: Getting upset person counts for conceptId: ", conceptId)
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    conceptId |> checkmate::assertIntegerish(lower = 1)
    yearsRange |> checkmate::assertIntegerish(len = 2, null.ok = TRUE)
    if (!is.null(yearsRange) && yearsRange[1] > yearsRange[2]) {
        stop("yearsRange: first year must be <= second year")
    }
    level |> checkmate::assertIntegerish(lower = 0, null.ok = TRUE)
    sexStratum |> checkmate::assertIntegerish(null.ok = TRUE)
    ageStratum |> checkmate::assertIntegerish(null.ok = TRUE)
    visitStratum |> checkmate::assertIntegerish(null.ok = TRUE)
    codeCountsTable |> checkmate::assertString()

    stratifiedPersonsTable <- "stratified_persons"

    connection <- CDMdbHandler$connectionHandler$getConnection()
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    # - Get the concept tree (shared with getPersonCountsFilters / getConceptRelationships, memoised)
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

    # - Exact set-overlap regions for the concepts at/under `level`, restricted to
    #   yearsRange and the requested strata. Pull the (person, target concept)
    #   membership pairs and build the exclusive-region key in R — the membership
    #   pull is bounded by the (small, user-picked) concept set.
    strataFilterSql <- paste0(
        .inFilterSql("gender_concept_id", sexStratum),
        .inFilterSql("age_decile", ageStratum),
        .betweenFilterSql("calendar_year", yearsRange),
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

    return(upsetPersonCounts)
}

# Builds " AND @column IN (v1,v2,...)", or "" when values is NULL/empty.
# values are always integer ids validated by checkmate upstream — safe to splice.
.inFilterSql <- function(column, values) {
    if (is.null(values) || length(values) == 0) {
        return("")
    }
    paste0(" AND ", column, " IN (", paste(as.integer(values), collapse = ","), ")")
}

#' Memoised version of getPersonCountsUpset
#'
#' @description
#' A memoised version of the getPersonCountsUpset function that caches results to improve
#' performance for repeated calls with the same parameters. The CDMdbHandler argument is
#' omitted from the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get person counts for
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`. NULL uses the full range.
#' @param level Maximum tree depth to include. NULL includes the full tree.
#' @param sexStratum Optional integer vector of `gender_concept_id` values to restrict to.
#' @param ageStratum Optional integer vector of `age_decile` values to restrict to.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values to restrict to.
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @importFrom memoise memoise
#'
#' @return Same shape as \code{\link{getPersonCountsUpset}}.
#'
#' @export
getPersonCountsUpset_memoise <- memoise::memoise(
    getPersonCountsUpset,
    omit_args = "CDMdbHandler"
)
