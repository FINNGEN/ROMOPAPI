#' Build an " AND <column> IN (...)" SQL fragment
#'
#' @description
#' Returns an empty string when `values` is NULL or empty, so the fragment can be
#' pasted unconditionally into a WHERE clause. Values are always integer ids
#' validated by checkmate upstream — safe to splice.
#'
#' Shared by `getPersonCountsUpset()`, `getPersonCountsFilters()` and
#' `getMeasurementValueHistogram()`.
#'
#' @param column Name of the column to filter on.
#' @param values Integer vector of values to keep, or NULL.
#'
#' @return A character scalar: the SQL fragment, or "".
#'
.inFilterSql <- function(column, values) {
    if (is.null(values) || length(values) == 0) {
        return("")
    }
    paste0(" AND ", column, " IN (", paste(as.integer(values), collapse = ","), ")")
}

#' Build an " AND <column> BETWEEN v1 AND v2" SQL fragment
#'
#' @description
#' Returns an empty string when `range` is NULL or empty, so the fragment can be
#' pasted unconditionally into a WHERE clause. `range` is always a length-2
#' integer vector validated by checkmate upstream — safe to splice.
#'
#' Shared by `getPersonCountsUpset()`, `getPersonCountsFilters()` and
#' `getMeasurementValueHistogram()`.
#'
#' @param column Name of the column to filter on.
#' @param range Length-2 integer vector `c(start, end)`, or NULL.
#'
#' @return A character scalar: the SQL fragment, or "".
#'
.betweenFilterSql <- function(column, range) {
    if (is.null(range) || length(range) == 0) {
        return("")
    }
    paste0(" AND ", column, " BETWEEN ", as.integer(range[1]), " AND ", as.integer(range[2]))
}
