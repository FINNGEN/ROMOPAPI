#' Get per-year incidence counts for a list of tagged concept sets
#'
#' @description
#' Computes, for each tagged concept set in `conceptIds` and each calendar year,
#' the exact distinct-person numerator counting a person only **once** -- in
#' the year of their first-ever record of that set's id set (the concept
#' itself or any requested descendant), read from the `stratified_persons`
#' bridge table -- alongside the same population-at-risk denominator
#' `getPersonCountsPrevalence()` uses (the precomputed
#' `observed_persons_counts_stratified` table -- see
#' `createObservedPersonsCountsTable()`). Dividing `person_counts` by
#' `observed_persons_counts` (x100) gives the per-year incidence rate; this
#' function returns the raw counts, not the rate.
#'
#' A person's first-ever record is computed once, over all history, ignoring
#' every filter below -- `sexStratum`/`ageStratum`/`visitStratum`/`yearsRange`
#' only decide whether that already-fixed incident event qualifies for the
#' count; they never change which year counts as "first" (see
#' `.buildIncidentRowsSql()`). One consequence: narrowing `yearsRange` can
#' legitimately drop a token to all-zero rows if every person's true first
#' year falls outside the requested range -- unlike
#' \code{\link{getPersonCountsPrevalence}}, where every year in range can show
#' a nonzero count.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Comma-separated list of tagged concept references, e.g.
#'   `"317009S,4191479SD,2000403993M,2000403993MD"`. See
#'   \code{\link{getPersonCountsUpset}} for the tag grammar.
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`, restricting
#'   the returned years to that inclusive calendar-year range. NULL or empty (default) uses the
#'   full range.
#' @param sexStratum Optional integer vector of `gender_concept_id` values to restrict to.
#'   NULL or empty (default) includes all.
#' @param ageStratum Optional integer vector of `age_decile` values to restrict to.
#'   NULL or empty (default) includes all.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values to
#'   restrict to. NULL or empty (default) includes all. Narrows the numerator only.
#'
#' @return A tibble of `tagged_concept_id` (the tagged token from `conceptIds`),
#'   `calendar_year`, `person_counts` (numerator, first-occurrence only) and
#'   `observed_persons_counts` (denominator) -- one row per tagged token x
#'   calendar year with a non-zero denominator.
#'
#' @importFrom checkmate assertClass assertString assertIntegerish
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate left_join coalesce select arrange filter
#' @importFrom tidyr expand_grid
#'
#' @export
getPersonCountsIncidence <- function(
    CDMdbHandler,
    conceptIds,
    yearsRange = NULL,
    sexStratum = NULL,
    ageStratum = NULL,
    visitStratum = NULL) {
    ParallelLogger::logInfo("getPersonCountsIncidence: Getting incidence person counts for conceptIds: ", conceptIds)
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
    observedPersonsCountsTable <- "observed_persons_counts_stratified"

    connection <- CDMdbHandler$connectionHandler$getConnection()
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    # - Parse and resolve each tagged token to its (column, id set) independently
    parsedTokens <- .parsePersonCountsConceptIds(conceptIds)
    resolvedTokens <- .resolveTaggedConceptIdSets(CDMdbHandler, parsedTokens)

    # - Numerator: count each person once, in their first-ever incident year
    #   (computed by .buildIncidentRowsSql() ignoring every filter), then
    #   restrict by the requested strata -- never by the MIN itself
    numeratorStrataFilterSql <- paste0(
        .inFilterSql("gender_concept_id", sexStratum),
        .inFilterSql("age_decile", ageStratum),
        .betweenFilterSql("calendar_year", yearsRange),
        .inFilterSql("visit_group_concept_id", visitStratum)
    )

    numeratorSql <- paste0(
        "SELECT tagged_concept_id, calendar_year, COUNT(DISTINCT person_id) AS person_counts
         FROM (", .buildIncidentRowsSql(resolvedTokens), ") incident_rows
         WHERE 1=1", numeratorStrataFilterSql, "
         GROUP BY tagged_concept_id, calendar_year;"
    )

    numerator <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = numeratorSql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedPersonsTable = stratifiedPersonsTable
    ) |>
        tibble::as_tibble()
    names(numerator) <- tolower(names(numerator))
    # an empty result set (e.g. a visitStratum nobody matches) comes back
    # untyped from some backends -- pin the types explicitly rather than
    # letting an empty logical column break the join below
    numerator <- numerator |>
        dplyr::mutate(
            tagged_concept_id = as.character(tagged_concept_id),
            calendar_year = as.integer(calendar_year),
            person_counts = as.integer(person_counts)
        )

    # - Denominator: population-at-risk per year, restricted to sex/age/year
    #   only (visitStratum does not narrow it -- see @description), identical
    #   to getPersonCountsPrevalence()'s denominator query
    denominatorStrataFilterSql <- paste0(
        .inFilterSql("gender_concept_id", sexStratum),
        .inFilterSql("age_decile", ageStratum),
        .betweenFilterSql("calendar_year", yearsRange)
    )

    denominatorSql <- paste0("
        SELECT calendar_year, SUM(observed_persons_counts) AS observed_persons_counts
        FROM @resultsDatabaseSchema.@observedPersonsCountsTable
        WHERE 1=1", denominatorStrataFilterSql, "
        GROUP BY calendar_year;
    ")

    denominator <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = denominatorSql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        observedPersonsCountsTable = observedPersonsCountsTable
    ) |>
        tibble::as_tibble()
    names(denominator) <- tolower(names(denominator))
    denominator <- denominator |>
        dplyr::mutate(
            calendar_year = as.integer(calendar_year),
            observed_persons_counts = as.integer(observed_persons_counts)
        )

    # - Complete the grid (every token x every denominator year) so a token with
    #   no incident persons in a year still gets a 0 row, then join in the
    #   denominator. Years without a denominator can't form a rate and are dropped.
    incidencePersonCounts <- tidyr::expand_grid(
        tagged_concept_id = resolvedTokens$token,
        calendar_year = denominator$calendar_year
    ) |>
        dplyr::left_join(numerator, by = c("tagged_concept_id", "calendar_year")) |>
        dplyr::mutate(person_counts = dplyr::coalesce(person_counts, 0L)) |>
        dplyr::left_join(denominator, by = "calendar_year") |>
        dplyr::filter(!is.na(observed_persons_counts), observed_persons_counts > 0) |>
        dplyr::arrange(tagged_concept_id, calendar_year) |>
        dplyr::select(tagged_concept_id, calendar_year, person_counts, observed_persons_counts)

    return(incidencePersonCounts)
}

#' Memoised version of getPersonCountsIncidence
#'
#' @description
#' A memoised version of the getPersonCountsIncidence function that caches results to improve
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
#' @return Same shape as \code{\link{getPersonCountsIncidence}}.
#'
#' @export
getPersonCountsIncidence_memoise <- memoise::memoise(
    getPersonCountsIncidence,
    omit_args = "CDMdbHandler"
)
