#' Get person-count breakdowns by sex, age, visit type and year for first-time incident events
#'
#' @description
#' Computes exact distinct-person breakdowns, by sex/age/visit-source-group/calendar-year,
#' for the pooled population of an explicit list of tagged concept references -- but counting
#' each person only **once**, in the year of their first-ever record of that set's id set (the
#' concept itself or any requested descendant), rather than every occurrence. Reads the
#' `stratified_persons` person-level bridge table through the same per-token "first occurrence"
#' logic as \code{\link{getPersonCountsIncidence}} (`.buildIncidentRowsSql()`), pooled across
#' every tagged set in `conceptIds` exactly like \code{\link{getPersonCountsFilters}} pools raw
#' occurrences.
#'
#' As in \code{\link{getPersonCountsFilters}}, each of the four dimensions (sex, age, visit,
#' year) is computed with the *other three* dimensions' filters applied, but not its own. A
#' person's first-ever record is fixed ignoring every filter -- the filters only decide whether
#' that fixed incident event qualifies, they never change which year counts as "first".
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Comma-separated list of tagged concept references. See
#'   \code{\link{getPersonCountsUpset}} for the tag grammar.
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`, marking
#'   which `year` strata are `selected`, and applied when computing the `sex`/`age`/`visit`
#'   breakdowns. NULL or empty (default) selects nothing and applies no year restriction.
#' @param sexStratum Optional integer vector of `gender_concept_id` values, marking which
#'   `sex` strata are `selected`, and applied when computing the `age`/`visit`/`year`
#'   breakdowns. NULL or empty (default) selects nothing and applies no restriction.
#' @param ageStratum Optional integer vector of `age_decile` values, marking which `age`
#'   strata are `selected`, and applied when computing the `sex`/`visit`/`year` breakdowns.
#'   NULL or empty (default) selects nothing and applies no restriction.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values, marking
#'   which `visit` strata are `selected`, and applied when computing the `sex`/`age`/`year`
#'   breakdowns. NULL or empty (default) selects nothing and applies no restriction.
#'
#' @return A tibble of `filter` ("sex"/"age"/"visit"/"year"), `stratum`, `person_counts`,
#'   `selected` -- a breakdown of the pooled first-incident-event population by each
#'   filterable dimension, each computed with the other three dimensions' filters applied,
#'   with `selected` marking the stratum value(s) that were part of that dimension's own filter.
#'
#' @importFrom checkmate assertClass assertString assertIntegerish
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate case_when
#'
#' @export
getPersonCountsIncidenceFilters <- function(
    CDMdbHandler,
    conceptIds,
    yearsRange = NULL,
    sexStratum = NULL,
    ageStratum = NULL,
    visitStratum = NULL) {
    ParallelLogger::logInfo("getPersonCountsIncidenceFilters: Getting incidence person count filters for conceptIds: ", conceptIds)
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

    # - Parse and resolve each tagged token to its (column, id set)
    parsedTokens <- .parsePersonCountsConceptIds(conceptIds)
    resolvedTokens <- .resolveTaggedConceptIdSets(CDMdbHandler, parsedTokens)

    sexFilterSql <- .inFilterSql("gender_concept_id", sexStratum)
    ageFilterSql <- .inFilterSql("age_decile", ageStratum)
    visitFilterSql <- .inFilterSql("visit_group_concept_id", visitStratum)
    yearFilterSql <- .betweenFilterSql("calendar_year", yearsRange)

    # - Pool every token's first-occurrence rows into one population: dropping
    #   tagged_concept_id in the outer SELECT DISTINCT is what collapses a
    #   person incident under two tokens, in the same year/sex/age/visit, to
    #   one row -- the same collapsing getPersonCountsFilters() already relies
    #   on for raw occurrences matching more than one requested set.
    sql <- paste0("
        WITH tree_persons AS (
            SELECT DISTINCT person_id, gender_concept_id, age_decile, visit_group_concept_id, calendar_year
            FROM (", .buildIncidentRowsSql(resolvedTokens), ") incident_rows
        )
        SELECT 'sex' AS filter, CAST(gender_concept_id AS BIGINT) AS stratum, COUNT(DISTINCT person_id) AS person_counts
        FROM tree_persons WHERE 1=1", ageFilterSql, visitFilterSql, yearFilterSql, "
        GROUP BY gender_concept_id
        UNION ALL
        SELECT 'age' AS filter, CAST(age_decile AS BIGINT) AS stratum, COUNT(DISTINCT person_id) AS person_counts
        FROM tree_persons WHERE 1=1", sexFilterSql, visitFilterSql, yearFilterSql, "
        GROUP BY age_decile
        UNION ALL
        SELECT 'visit' AS filter, CAST(visit_group_concept_id AS BIGINT) AS stratum, COUNT(DISTINCT person_id) AS person_counts
        FROM tree_persons WHERE 1=1", sexFilterSql, ageFilterSql, yearFilterSql, "
        GROUP BY visit_group_concept_id
        UNION ALL
        SELECT 'year' AS filter, CAST(calendar_year AS BIGINT) AS stratum, COUNT(DISTINCT person_id) AS person_counts
        FROM tree_persons WHERE 1=1", sexFilterSql, ageFilterSql, visitFilterSql, "
        GROUP BY calendar_year;
    ")
    filterPersonCounts <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedPersonsTable = stratifiedPersonsTable
    ) |>
        tibble::as_tibble()
    names(filterPersonCounts) <- tolower(names(filterPersonCounts))

    # `selected` marks the stratum value(s) that were part of that dimension's own
    # filter — sex/age/visit are membership checks, year is a range check (computed
    # separately to avoid comparing against a possibly-NULL yearsRange in case_when).
    yearSelected <- if (is.null(yearsRange)) {
        rep(FALSE, nrow(filterPersonCounts))
    } else {
        filterPersonCounts$filter == "year" &
            filterPersonCounts$stratum >= yearsRange[1] &
            filterPersonCounts$stratum <= yearsRange[2]
    }

    filterPersonCounts <- filterPersonCounts |>
        dplyr::mutate(
            selected = dplyr::case_when(
                filter == "sex" & stratum %in% sexStratum ~ TRUE,
                filter == "age" & stratum %in% ageStratum ~ TRUE,
                filter == "visit" & stratum %in% visitStratum ~ TRUE,
                TRUE ~ FALSE
            ) | yearSelected
        )

    return(filterPersonCounts)
}

#' Memoised version of getPersonCountsIncidenceFilters
#'
#' @description
#' A memoised version of the getPersonCountsIncidenceFilters function that caches results to
#' improve performance for repeated calls with the same parameters. The CDMdbHandler argument
#' is omitted from the cache key to allow sharing across different database connections.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Comma-separated list of tagged concept references. See
#'   \code{\link{getPersonCountsUpset}} for the tag grammar.
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`.
#' @param sexStratum Optional integer vector of `gender_concept_id` values.
#' @param ageStratum Optional integer vector of `age_decile` values.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values.
#'
#' @importFrom memoise memoise
#'
#' @return Same shape as \code{\link{getPersonCountsIncidenceFilters}}.
#'
#' @export
getPersonCountsIncidenceFilters_memoise <- memoise::memoise(
    getPersonCountsIncidenceFilters,
    omit_args = "CDMdbHandler"
)
