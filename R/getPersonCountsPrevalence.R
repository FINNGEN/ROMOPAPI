#' Get per-year prevalence counts for a list of tagged concept sets
#'
#' @description
#' Computes, for each tagged concept set in `conceptIds` and each calendar year,
#' the exact distinct-person numerator (persons matching that set's id set and
#' the requested strata in that year, read from the `stratified_persons` bridge
#' table) alongside the population-at-risk denominator (persons under
#' observation in that year, read from the precomputed
#' `observed_persons_counts_stratified` table — see
#' `createObservedPersonsCountsTable()`). Dividing `person_counts` by
#' `observed_persons_counts` (x100) gives the per-year prevalence; this function
#' returns the raw counts, not the rate, so the ratio stays auditable.
#'
#' `sexStratum`/`ageStratum`/`yearsRange` narrow both the numerator and the
#' denominator exactly (a person has exactly one sex and one age decile per
#' year, so the denominator stays exact under these filters). `visitStratum`
#' narrows the numerator **only** — the denominator table has no visit
#' dimension, since the same person can have events in more than one visit
#' group in the same year, which would make a visit-conditioned denominator
#' inexact as a simple per-stratum sum.
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
#'   `calendar_year`, `person_counts` (numerator) and `observed_persons_counts`
#'   (denominator) — one row per tagged token x calendar year with a non-zero
#'   denominator.
#'
#' @importFrom checkmate assertClass assertString assertIntegerish
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate left_join coalesce select arrange filter
#' @importFrom tidyr expand_grid
#' @importFrom purrr pmap_chr
#'
#' @export
getPersonCountsPrevalence <- function(
    CDMdbHandler,
    conceptIds,
    yearsRange = NULL,
    sexStratum = NULL,
    ageStratum = NULL,
    visitStratum = NULL) {
    ParallelLogger::logInfo("getPersonCountsPrevalence: Getting prevalence person counts for conceptIds: ", conceptIds)
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

    # - Numerator: per-token, per-year distinct-person counts, restricted to
    #   all four strata dimensions
    numeratorStrataFilterSql <- paste0(
        .inFilterSql("gender_concept_id", sexStratum),
        .inFilterSql("age_decile", ageStratum),
        .betweenFilterSql("calendar_year", yearsRange),
        .inFilterSql("visit_group_concept_id", visitStratum)
    )

    tokenQueries <- resolvedTokens |>
        purrr::pmap_chr(function(token, column, resolved_ids, ...) {
            paste0(
                "SELECT '", token, "' AS tagged_concept_id, calendar_year,
                        COUNT(DISTINCT person_id) AS person_counts
                 FROM @resultsDatabaseSchema.@stratifiedPersonsTable
                 WHERE ", column, " IN (", paste(resolved_ids, collapse = ","), ")", numeratorStrataFilterSql, "
                 GROUP BY calendar_year"
            )
        })

    numeratorSql <- paste0(paste(tokenQueries, collapse = " UNION ALL "), ";")

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
    #   only (visitStratum does not narrow it -- see @description)
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
    #   no events in a year still gets a 0 row, then join in the denominator.
    #   Years without a denominator can't form a prevalence and are dropped.
    prevalencePersonCounts <- tidyr::expand_grid(
        tagged_concept_id = resolvedTokens$token,
        calendar_year = denominator$calendar_year
    ) |>
        dplyr::left_join(numerator, by = c("tagged_concept_id", "calendar_year")) |>
        dplyr::mutate(person_counts = dplyr::coalesce(person_counts, 0L)) |>
        dplyr::left_join(denominator, by = "calendar_year") |>
        dplyr::filter(!is.na(observed_persons_counts), observed_persons_counts > 0) |>
        dplyr::arrange(tagged_concept_id, calendar_year) |>
        dplyr::select(tagged_concept_id, calendar_year, person_counts, observed_persons_counts)

    return(prevalencePersonCounts)
}

#' Memoised version of getPersonCountsPrevalence
#'
#' @description
#' A memoised version of the getPersonCountsPrevalence function that caches results to improve
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
#' @return Same shape as \code{\link{getPersonCountsPrevalence}}.
#'
#' @export
getPersonCountsPrevalence_memoise <- memoise::memoise(
    getPersonCountsPrevalence,
    omit_args = "CDMdbHandler"
)
