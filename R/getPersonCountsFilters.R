#' Get person-count breakdowns by sex, age and visit type for a concept tree
#'
#' @description
#' Computes exact distinct-person breakdowns, by sex/age/visit-source-group, for the
#' concept tree rooted at `conceptId` (the concept and all its descendants), reading the
#' `stratified_persons` person-level bridge table (see `createStratifiedPersonsTable()`).
#' Reuses the memoised tree getter so the tree is only computed once across repeated calls.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptId The concept ID to get person counts for
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`, restricting
#'   the breakdown to that inclusive calendar-year range. NULL or empty (default) uses the
#'   full range.
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
#'
#' @return A tibble of `filter` ("sex"/"age"/"visit"), `stratum`, `person_counts` — a
#'   breakdown of the tree's persons (concept + all descendants) by each filterable
#'   dimension, over `yearsRange`. Lets a client discover which strata are worth
#'   filtering by, and how many persons they hold.
#'
#' @importFrom checkmate assertClass assertIntegerish assertString
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#'
#' @export
getPersonCountsFilters <- function(
    CDMdbHandler,
    conceptId,
    yearsRange = NULL,
    codeCountsTable = "code_counts") {
    ParallelLogger::logInfo("getPersonCountsFilters: Getting person count filters for conceptId: ", conceptId)
    #
    # VALIDATE
    #
    CDMdbHandler |> checkmate::assertClass("CDMdbHandler")
    conceptId |> checkmate::assertIntegerish(lower = 1)
    yearsRange |> checkmate::assertIntegerish(len = 2, null.ok = TRUE)
    if (!is.null(yearsRange) && yearsRange[1] > yearsRange[2]) {
        stop("yearsRange: first year must be <= second year")
    }
    codeCountsTable |> checkmate::assertString()

    stratifiedPersonsTable <- "stratified_persons"

    connection <- CDMdbHandler$connectionHandler$getConnection()
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    # - Get the concept tree (shared with getPersonCountsUpset / getConceptRelationships, memoised)
    conceptTree <- getConceptTree_memoise(CDMdbHandler, conceptId = conceptId, codeCountsTable = codeCountsTable)
    treeConceptIds <- conceptTree$concept_ids

    # - Breakdown of the tree's (concept + all descendants) persons by sex/age/visit,
    #   over yearsRange (full range when NULL).
    yearFilterSql <- .betweenFilterSql("calendar_year", yearsRange)

    sql <- paste0("
        WITH tree_persons AS (
            SELECT DISTINCT person_id, gender_concept_id, age_decile, visit_group_concept_id
            FROM @resultsDatabaseSchema.@stratifiedPersonsTable
            WHERE (concept_id IN (@conceptIds) OR maps_to_concept_id IN (@conceptIds))", yearFilterSql, "
        )
        SELECT 'sex' AS filter, CAST(gender_concept_id AS BIGINT) AS stratum, COUNT(DISTINCT person_id) AS person_counts
        FROM tree_persons GROUP BY gender_concept_id
        UNION ALL
        SELECT 'age' AS filter, CAST(age_decile AS BIGINT) AS stratum, COUNT(DISTINCT person_id) AS person_counts
        FROM tree_persons GROUP BY age_decile
        UNION ALL
        SELECT 'visit' AS filter, CAST(visit_group_concept_id AS BIGINT) AS stratum, COUNT(DISTINCT person_id) AS person_counts
        FROM tree_persons GROUP BY visit_group_concept_id;
    ")
    filterPersonCounts <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedPersonsTable = stratifiedPersonsTable,
        conceptIds = paste(treeConceptIds, collapse = ",")
    ) |>
        tibble::as_tibble()
    names(filterPersonCounts) <- tolower(names(filterPersonCounts))

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
#' @param conceptId The concept ID to get person counts for
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`. NULL uses the full range.
#' @param codeCountsTable Name of the code counts table in the results schema. Defaults to "code_counts"
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
