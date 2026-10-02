#' Get exact set-overlap (UpSet) person counts for a list of tagged concept sets
#'
#' @description
#' Computes exact UpSet exclusive-region person counts for an explicit list of
#' tagged concept references, reading the `stratified_persons` person-level bridge
#' table (see `createStratifiedPersonsTable()`). Each item in `conceptIds` is an
#' independent set: `<conceptId><S|M><D?>` — `S`/`M` picks which column to match
#' (`concept_id` vs `maps_to_concept_id`), and an optional trailing `D` expands the
#' set to the concept and all its descendants (via `concept_ancestor`, including
#' the concept itself) instead of matching the code directly. Two tokens that share
#' the same concept id but differ in tag (e.g. `"2000403993M"` and `"2000403993MD"`)
#' are independent sets.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Comma-separated list of tagged concept references, e.g.
#'   `"317009S,4191479SD,2000403993M,2000403993MD"`. See description for the tag grammar.
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`, restricting
#'   the regions to that inclusive calendar-year range. NULL or empty (default) uses the
#'   full range.
#' @param sexStratum Optional integer vector of `gender_concept_id` values to restrict to.
#'   NULL or empty (default) includes all.
#' @param ageStratum Optional integer vector of `age_decile` values to restrict to.
#'   NULL or empty (default) includes all.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values to
#'   restrict to. NULL or empty (default) includes all.
#'
#' @return A tibble of `group` (the tagged tokens of the exclusive region, joined by "-")
#'   and `person_counts` — exact UpSet exclusive-region counts for the given concept
#'   sets, restricted to `yearsRange` and the requested strata.
#'
#' @importFrom checkmate assertClass assertString assertIntegerish
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by summarise count
#' @importFrom purrr pmap_chr
#'
#' @export
getPersonCountsUpset <- function(
    CDMdbHandler,
    conceptIds,
    yearsRange = NULL,
    sexStratum = NULL,
    ageStratum = NULL,
    visitStratum = NULL) {
    ParallelLogger::logInfo("getPersonCountsUpset: Getting upset person counts for conceptIds: ", conceptIds)
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    conceptIds |> checkmate::assertString()
    yearsRange |> checkmate::assertIntegerish(len = 2, any.missing = FALSE, null.ok = TRUE)
    if (!is.null(yearsRange) && yearsRange[1] > yearsRange[2]) {
        stop("yearsRange: first year must be <= second year")
    }
    sexStratum |> checkmate::assertIntegerish(any.missing = FALSE, null.ok = TRUE)
    ageStratum |> checkmate::assertIntegerish(any.missing = FALSE, null.ok = TRUE)
    visitStratum |> checkmate::assertIntegerish(any.missing = FALSE, null.ok = TRUE)

    stratifiedPersonsTable <- "stratified_persons"

    connection <- CDMdbHandler$connectionHandler$getConnection()
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    # - Parse and resolve each tagged token to its (column, id set) independently
    parsedTokens <- .parsePersonCountsConceptIds(conceptIds)
    resolvedTokens <- .resolveTaggedConceptIdSets(CDMdbHandler, parsedTokens)

    # - Exact set-overlap regions for the given concept sets, restricted to
    #   yearsRange and the requested strata. Pull the (person, set label)
    #   membership pairs and build the exclusive-region key in R.
    strataFilterSql <- paste0(
        .inFilterSql("gender_concept_id", sexStratum),
        .inFilterSql("age_decile", ageStratum),
        .betweenFilterSql("calendar_year", yearsRange),
        .inFilterSql("visit_group_concept_id", visitStratum)
    )

    tokenQueries <- resolvedTokens |>
        purrr::pmap_chr(function(token, column, resolved_ids, ...) {
            paste0(
                "SELECT DISTINCT person_id, '", token, "' AS target_set
                 FROM @resultsDatabaseSchema.@stratifiedPersonsTable
                 WHERE ", column, " IN (", paste(resolved_ids, collapse = ","), ")", strataFilterSql
            )
        })

    sql <- paste0(paste(tokenQueries, collapse = " UNION ALL "), ";")

    membership <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedPersonsTable = stratifiedPersonsTable
    ) |>
        tibble::as_tibble()
    names(membership) <- tolower(names(membership))

    upsetPersonCounts <- membership |>
        dplyr::group_by(person_id) |>
        dplyr::summarise(
            group = paste(sort(unique(target_set)), collapse = "-"),
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
#' @param conceptIds Comma-separated list of tagged concept references. See
#'   \code{\link{getPersonCountsUpset}} for the tag grammar.
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`. NULL uses the full range.
#' @param sexStratum Optional integer vector of `gender_concept_id` values to restrict to.
#' @param ageStratum Optional integer vector of `age_decile` values to restrict to.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values to restrict to.
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
