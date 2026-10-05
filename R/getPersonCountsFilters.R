#' Get person-count breakdowns by sex, age, visit type and year for a list of tagged concept sets
#'
#' @description
#' Computes exact distinct-person breakdowns, by sex/age/visit-source-group/calendar-year,
#' for the pooled population of an explicit list of tagged concept references, reading the
#' `stratified_persons` person-level bridge table (see `createStratifiedPersonsTable()`).
#' Each item in `conceptIds` uses the same tag grammar as `getPersonCountsUpset()`
#' (`<conceptId><S|M><D?>`), but here every set is pooled into one combined population
#' rather than reported as separate overlapping regions.
#'
#' Each of the four dimensions (sex, age, visit, year) is computed with the *other three*
#' dimensions' filters applied, but not its own — so a client can see, for example, how
#' the sex breakdown looks under the current age/visit/year filters, while still being told
#' which sex value(s) were actually selected (`selected` column).
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
#'   `selected` — a breakdown of the pooled population's persons by each filterable
#'   dimension, each computed with the other three dimensions' filters applied, with
#'   `selected` marking the stratum value(s) that were part of that dimension's own filter.
#'
#' @importFrom checkmate assertClass assertString assertIntegerish
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate case_when
#' @importFrom purrr pmap_chr
#'
#' @export
getPersonCountsFilters <- function(
    CDMdbHandler,
    conceptIds,
    yearsRange = NULL,
    sexStratum = NULL,
    ageStratum = NULL,
    visitStratum = NULL) {
    ParallelLogger::logInfo("getPersonCountsFilters: Getting person count filters for conceptIds: ", conceptIds)
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

    # - Pool all sets into one population: OR their (column, ids) predicates together
    populationPredicate <- resolvedTokens |>
        purrr::pmap_chr(function(column, resolved_ids, ...) {
            paste0(column, " IN (", paste(resolved_ids, collapse = ","), ")")
        }) |>
        paste(collapse = " OR ")

    sexFilterSql <- .inFilterSql("gender_concept_id", sexStratum)
    ageFilterSql <- .inFilterSql("age_decile", ageStratum)
    visitFilterSql <- .inFilterSql("visit_group_concept_id", visitStratum)
    yearFilterSql <- .betweenFilterSql("calendar_year", yearsRange)

    sql <- paste0("
        WITH tree_persons AS (
            SELECT DISTINCT person_id, gender_concept_id, age_decile, visit_group_concept_id, calendar_year
            FROM @resultsDatabaseSchema.@stratifiedPersonsTable
            WHERE (", populationPredicate, ")
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

# Builds " AND @column BETWEEN v1 AND v2", or "" when range is NULL/empty.
# range is always a length-2 integer vector validated by checkmate upstream — safe to splice.
.betweenFilterSql <- function(column, range) {
    if (is.null(range) || length(range) == 0) {
        return("")
    }
    paste0(" AND ", column, " BETWEEN ", as.integer(range[1]), " AND ", as.integer(range[2]))
}

#' Memoised version of getPersonCountsFilters
#'
#' @description
#' A memoised version of the getPersonCountsFilters function that caches results to improve
#' performance for repeated calls with the same parameters. The CDMdbHandler argument is
#' omitted from the cache key to allow sharing across different database connections.
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
#' @return Same shape as \code{\link{getPersonCountsFilters}}.
#'
#' @export
getPersonCountsFilters_memoise <- memoise::memoise(
    getPersonCountsFilters,
    omit_args = "CDMdbHandler"
)
