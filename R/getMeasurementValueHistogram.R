#' Get a histogram of measured values for a list of tagged concept sets
#'
#' @description
#' Computes a histogram of `value_as_number` for an explicit list of tagged
#' concept references, reading the `stratified_measurements` table (see
#' `createStratifiedMeasurementsTable()`). Only `Measurement`-domain concepts are
#' accepted. The tag grammar is the same as `getPersonCountsUpset()`:
#' `<conceptId><S|M><D?>` — `S`/`M` picks which column to match (`concept_id` vs
#' `maps_to_concept_id`), and a trailing `D` expands the set to the concept and
#' all its descendants.
#'
#' Bins are computed **per unit**, pooled across every entry in `conceptIds` that shares
#' that unit. So entries recorded in the same unit come back on one shared set of buckets
#' — one histogram, with one series per `tagged_conceptid` that a client can stack as
#' coloured segments of the same bar — while entries in different units come back as
#' separate histograms, since their values are not comparable.
#'
#' Bin breaks use a robust `median +/- 7 * MAD` range rather than the raw
#' min/max, so a single extreme outlier cannot collapse every real value into one
#' bin. Values outside that range are kept in an underflow bin (`bin_index = -1`,
#' labelled `(-Inf, x]`) and an overflow bin (`bin_index = nBins`, labelled
#' `(x, +Inf]`). When the MAD is zero (all values identical) the range is widened
#' by a small fixed step so the division stays defined.
#'
#' The breaks are computed over the **unfiltered** events of each unit while
#' the counts are computed over the **filtered** ones, so the buckets stay fixed
#' as a client changes `yearsRange`/`sexStratum`/`ageStratum`/`visitStratum`.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Comma-separated list of tagged concept references, e.g.
#'   `"40652733S,40652733SD"`. All referenced concepts must be in the Measurement domain.
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`, restricting
#'   the counted events to that inclusive calendar-year range. NULL or empty (default) uses
#'   the full range.
#' @param sexStratum Optional integer vector of `gender_concept_id` values to restrict to.
#'   NULL or empty (default) includes all.
#' @param ageStratum Optional integer vector of `age_decile` values to restrict to.
#'   NULL or empty (default) includes all.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values to
#'   restrict to. NULL or empty (default) includes all.
#' @param nBins Number of bins to divide the robust range into. Defaults to 100. Two extra
#'   bins (underflow and overflow) are always returned on top of these.
#'
#' @return A tibble of `tagged_conceptid`, `measured_value_bucket`, `unit` and `n_events`.
#'   Every (tagged_conceptid, unit) series returns all `nBins + 2` bins, zero-filled where
#'   empty, ordered by tagged_conceptid, unit and ascending value. Series sharing a unit
#'   share their bucket labels.
#'
#' @importFrom checkmate assertClass assertString assertIntegerish assertCount
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr filter pull distinct left_join mutate select arrange
#' @importFrom purrr pmap_chr
#' @importFrom tidyr unnest replace_na
#'
#' @export
getMeasurementValueHistogram <- function(
    CDMdbHandler,
    conceptIds,
    yearsRange = NULL,
    sexStratum = NULL,
    ageStratum = NULL,
    visitStratum = NULL,
    nBins = 100) {
    ParallelLogger::logInfo(
        "getMeasurementValueHistogram: Getting value histogram for conceptIds: ", conceptIds
    )
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
    nBins |> checkmate::assertCount(positive = TRUE)
    nBins <- as.integer(nBins)

    stratifiedMeasurementsTable <- "stratified_measurements"

    connection <- CDMdbHandler$connectionHandler$getConnection()
    resultsDatabaseSchema <- CDMdbHandler$resultsDatabaseSchema

    #
    # FUNCTION
    #

    # - Parse and resolve each tagged token to its (column, id set) independently
    parsedTokens <- .parsePersonCountsConceptIds(conceptIds)
    .assertMeasurementDomain(CDMdbHandler, parsedTokens$concept_id)
    resolvedTokens <- .resolveTaggedConceptIdSets(CDMdbHandler, parsedTokens)

    # - Strata filters are applied to the counted events only, never to the events the
    #   bin breaks are derived from, so the buckets do not move when a filter changes.
    strataFilterSql <- paste0(
        .inFilterSql("gender_concept_id", sexStratum),
        .inFilterSql("age_decile", ageStratum),
        .betweenFilterSql("calendar_year", yearsRange),
        .inFilterSql("visit_group_concept_id", visitStratum)
    )

    tokenQueries <- resolvedTokens |>
        purrr::pmap_chr(function(token, column, resolved_ids, ...) {
            paste0(
                "SELECT '", token, "' AS target_set, unit_concept_id, value_as_number,
                        gender_concept_id, age_decile, calendar_year, visit_group_concept_id
                 FROM @resultsDatabaseSchema.@stratifiedMeasurementsTable
                 WHERE ", column, " IN (", paste(resolved_ids, collapse = ","), ")"
            )
        })

    sql <- .measurementHistogramSql(
        tokenUnionSql = paste(tokenQueries, collapse = " UNION ALL "),
        strataFilterSql = strataFilterSql
    )

    binCounts <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        stratifiedMeasurementsTable = stratifiedMeasurementsTable,
        n_bins = nBins,
        mad_factor = .MAD_FACTOR,
        soft_min_step = .SOFT_MIN_STEP
    ) |>
        tibble::as_tibble()
    names(binCounts) <- tolower(names(binCounts))

    if (nrow(binCounts) == 0) {
        return(tibble::tibble(
            tagged_conceptid = character(0),
            measured_value_bucket = character(0),
            unit = character(0),
            n_events = integer(0)
        ))
    }

    # - Zero-fill every bin of every series so a stacked bar chart has no gaps. A series is
    #   (tagged_conceptid, unit); the breaks are shared across every series of a unit, so
    #   the series of one unit all carry the SAME bucket labels and their bars stack.
    series <- binCounts |>
        dplyr::distinct(target_set, unit_concept_id, break_min, break_max)

    histogram <- series |>
        dplyr::mutate(bin_index = list(seq.int(-1L, nBins))) |>
        tidyr::unnest(cols = "bin_index") |>
        dplyr::left_join(
            binCounts |> dplyr::select(target_set, unit_concept_id, bin_index, n_events),
            by = c("target_set", "unit_concept_id", "bin_index")
        ) |>
        dplyr::mutate(n_events = tidyr::replace_na(n_events, 0L))

    # - Resolve the unit concept to its code; unit_concept_id 0 means "no unit recorded"
    unitNames <- .getUnitConceptCodes(CDMdbHandler, unique(histogram$unit_concept_id))

    histogram |>
        dplyr::left_join(unitNames, by = "unit_concept_id") |>
        dplyr::mutate(
            measured_value_bucket = .binLabel(bin_index, break_min, break_max, nBins),
            tagged_conceptid = target_set
        ) |>
        dplyr::arrange(tagged_conceptid, unit_concept_id, bin_index) |>
        dplyr::select(tagged_conceptid, measured_value_bucket, unit, n_events)
}

# Robust-range constants, matching the reference implementation this mirrors.
# A factor of 7 MADs covers ~98% of a typical lab-value distribution; the soft step
# keeps the range non-degenerate when every value in a partition is identical.
.MAD_FACTOR <- 7
.SOFT_MIN_STEP <- 0.01

#' Build the measured-value histogram SQL
#'
#' @description
#' One query that derives robust bin breaks (median, then MAD, then
#' `median +/- mad_factor * MAD`) per `unit_concept_id` over the unfiltered events, then
#' bins and counts the filtered events -- per `(target_set, unit_concept_id)` -- against
#' those breaks. Pooling the breaks across tokens of one unit is what aligns their buckets.
#'
#' Median is computed with `ROW_NUMBER()`/`COUNT() OVER` and the average of the middle
#' row(s) rather than `PERCENTILE_CONT`, which SQLite does not have at all and which
#' BigQuery spells differently — this form translates unchanged to every backend.
#'
#' Note: the bin index is materialised in its own CTE so the final `GROUP BY` lists only
#' plain column names. SqlRender mistranslates a `GROUP BY` that repeats a `CASE`
#' expression into positional form, silently dropping a grouping column on BigQuery.
#'
#' @param tokenUnionSql UNION ALL of one SELECT per token, each tagging its rows with
#'   a `target_set` literal.
#' @param strataFilterSql Pre-built " AND ..." fragments restricting the counted events.
#'
#' @return A character scalar of un-rendered SQL.
#'
.measurementHistogramSql <- function(tokenUnionSql, strataFilterSql) {
    paste0("
    WITH token_events AS (
        ", tokenUnionSql, "
    ),
    -- Breaks are derived per UNIT, pooled across every token that shares that unit, so
    -- all those tokens land on one x-axis and their bars can stack. (Values in different
    -- units are not comparable, so those stay separate histograms.)
    ranked AS (
        SELECT unit_concept_id, value_as_number,
               ROW_NUMBER() OVER (PARTITION BY unit_concept_id ORDER BY value_as_number) AS rn,
               COUNT(*) OVER (PARTITION BY unit_concept_id) AS cnt
        FROM token_events
    ),
    medians AS (
        SELECT unit_concept_id, AVG(value_as_number) AS median_value
        FROM ranked
        WHERE rn = FLOOR((cnt + 1) / 2.0) OR rn = FLOOR((cnt + 2) / 2.0)
        GROUP BY unit_concept_id
    ),
    deviations AS (
        SELECT t.unit_concept_id AS unit_concept_id,
               ABS(t.value_as_number - m.median_value) AS absolute_deviation
        FROM token_events t
        INNER JOIN medians m ON t.unit_concept_id = m.unit_concept_id
    ),
    ranked_deviations AS (
        SELECT unit_concept_id, absolute_deviation,
               ROW_NUMBER() OVER (PARTITION BY unit_concept_id ORDER BY absolute_deviation) AS rn,
               COUNT(*) OVER (PARTITION BY unit_concept_id) AS cnt
        FROM deviations
    ),
    mads AS (
        SELECT unit_concept_id, AVG(absolute_deviation) AS mad_value
        FROM ranked_deviations
        WHERE rn = FLOOR((cnt + 1) / 2.0) OR rn = FLOOR((cnt + 2) / 2.0)
        GROUP BY unit_concept_id
    ),
    breaks AS (
        SELECT m.unit_concept_id AS unit_concept_id,
               CASE WHEN d.mad_value = 0 THEN m.median_value - @soft_min_step
                    ELSE m.median_value - @mad_factor * d.mad_value END AS break_min,
               CASE WHEN d.mad_value = 0 THEN m.median_value + @soft_min_step
                    ELSE m.median_value + @mad_factor * d.mad_value END AS break_max
        FROM medians m
        INNER JOIN mads d ON m.unit_concept_id = d.unit_concept_id
    ),
    filtered_events AS (
        SELECT target_set, unit_concept_id, value_as_number
        FROM token_events
        WHERE 1 = 1", strataFilterSql, "
    ),
    scaled AS (
        SELECT f.target_set AS target_set, f.unit_concept_id AS unit_concept_id,
               b.break_min AS break_min, b.break_max AS break_max,
               @n_bins * (f.value_as_number - b.break_min) / (b.break_max - b.break_min) AS value_on_bin_scale
        FROM filtered_events f
        INNER JOIN breaks b ON f.unit_concept_id = b.unit_concept_id
    ),
    raw_index AS (
        SELECT target_set, unit_concept_id, break_min, break_max,
               -- right-closed bins: a value landing exactly on a break belongs to the bin below
               CASE WHEN value_on_bin_scale = FLOOR(value_on_bin_scale)
                    THEN FLOOR(value_on_bin_scale) - 1
                    ELSE FLOOR(value_on_bin_scale) END AS raw_idx
        FROM scaled
    ),
    binned AS (
        SELECT target_set, unit_concept_id, break_min, break_max,
               CAST(CASE WHEN raw_idx < 0 THEN -1
                         WHEN raw_idx >= @n_bins THEN @n_bins
                         ELSE raw_idx END AS INT) AS bin_index
        FROM raw_index
    )
    SELECT target_set, unit_concept_id, bin_index, break_min, break_max, COUNT(*) AS n_events
    FROM binned
    GROUP BY target_set, unit_concept_id, bin_index, break_min, break_max;
    ")
}

#' Assert that every concept is in the Measurement domain
#'
#' @description
#' One batched `concept` lookup. Errors listing the offending concept ids, so a caller
#' passing e.g. a Condition concept gets a usable message rather than an empty histogram.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Integer vector of concept ids to check.
#'
#' @return Invisibly TRUE; errors otherwise.
#'
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter pull
#'
.assertMeasurementDomain <- function(CDMdbHandler, conceptIds) {
    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema

    sql <- "
    SELECT c.concept_id AS concept_id, c.domain_id AS domain_id
    FROM @vocabularyDatabaseSchema.concept c
    WHERE c.concept_id IN (@conceptIds);
    "
    domains <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptIds = paste(unique(conceptIds), collapse = ",")
    ) |>
        tibble::as_tibble()
    names(domains) <- tolower(names(domains))

    missing <- setdiff(unique(conceptIds), domains$concept_id)
    if (length(missing) > 0) {
        stop("concept not found: ", paste(missing, collapse = ", "))
    }

    notMeasurement <- domains |>
        dplyr::filter(domain_id != "Measurement") |>
        dplyr::pull(concept_id)
    if (length(notMeasurement) > 0) {
        stop(
            "getMeasurementValueHistogram only accepts Measurement-domain concepts; ",
            "not in the Measurement domain: ", paste(notMeasurement, collapse = ", ")
        )
    }

    invisible(TRUE)
}

#' Look up the concept codes of unit concepts
#'
#' @description
#' Maps `unit_concept_id` to its `concept_code`. `unit_concept_id` 0 means no unit was
#' recorded, and resolves to NA rather than being dropped — unitless values (ratios,
#' scores) are still valid measurements.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param unitConceptIds Integer vector of unit concept ids.
#'
#' @return A tibble of `unit_concept_id`, `unit`.
#'
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr rename mutate
#'
.getUnitConceptCodes <- function(CDMdbHandler, unitConceptIds) {
    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema

    realUnitIds <- setdiff(unique(unitConceptIds), 0)
    if (length(realUnitIds) == 0) {
        return(tibble::tibble(unit_concept_id = numeric(0), unit = character(0)))
    }

    sql <- "
    SELECT c.concept_id AS unit_concept_id, c.concept_code AS unit
    FROM @vocabularyDatabaseSchema.concept c
    WHERE c.concept_id IN (@unitConceptIds);
    "
    units <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        unitConceptIds = paste(realUnitIds, collapse = ",")
    ) |>
        tibble::as_tibble()
    names(units) <- tolower(names(units))

    units
}

#' Build the "(x1, x2]" label for a bin index
#'
#' @description
#' Mirrors the bin-definition arithmetic of the reference implementation: the underflow
#' bin opens at -Inf, the overflow bin closes at +Inf, and the last in-range bin closes
#' exactly on `break_max` rather than on a re-derived edge.
#'
#' @param binIndex Integer vector of bin indices, `-1` to `nBins`.
#' @param breakMin Numeric vector of partition lower breaks.
#' @param breakMax Numeric vector of partition upper breaks.
#' @param nBins Number of in-range bins.
#'
#' @return A character vector of bucket labels.
#'
.binLabel <- function(binIndex, breakMin, breakMax, nBins) {
    binWidth <- (breakMax - breakMin) / nBins

    x1 <- breakMin + binIndex * binWidth
    x2 <- breakMin + (binIndex + 1) * binWidth

    # the outer bins are unbounded; the last in-range bin ends exactly on break_max
    x1 <- ifelse(binIndex == -1, -Inf, ifelse(binIndex == nBins, breakMax, x1))
    x2 <- ifelse(binIndex == nBins, Inf, ifelse(binIndex == nBins - 1, breakMax, x2))

    digits <- .labelDigits(binWidth)

    paste0("(", .formatBreak(x1, digits), ", ", .formatBreak(x2, digits), "]")
}

#' Decide how many decimals a bucket label needs
#'
#' @description
#' Two decimals as the reference implementation uses, but widened when the bins are
#' narrower than that can express. Without this, a low-dispersion partition (e.g. every
#' value identical, where the range is only `2 * soft_min_step` wide) renders several
#' different bins with the *same* label — which breaks any client keying a chart on the
#' bucket string.
#'
#' @param binWidth Numeric vector of per-partition bin widths.
#'
#' @return An integer vector of decimal counts, between 2 and 10.
#'
.labelDigits <- function(binWidth) {
    digits <- ifelse(
        is.finite(binWidth) & binWidth > 0,
        ceiling(-log10(binWidth)) + 1,
        2
    )
    pmin(pmax(digits, 2), 10)
}

#' Format a bin edge for a bucket label
#'
#' @param x Numeric vector of bin edges.
#' @param digits Integer vector of decimals to use, parallel to `x`.
#'
#' @return A character vector with infinities spelled out.
#'
.formatBreak <- function(x, digits) {
    vapply(
        seq_along(x),
        function(i) {
            if (is.infinite(x[i])) {
                if (x[i] < 0) "-Inf" else "+Inf"
            } else {
                formatC(x[i], format = "f", digits = digits[i])
            }
        },
        character(1)
    )
}

#' Memoised version of getMeasurementValueHistogram
#'
#' @description
#' A memoised version of the getMeasurementValueHistogram function that caches results to
#' improve performance for repeated calls with the same parameters. The CDMdbHandler
#' argument is omitted from the cache key to allow sharing across different database
#' connections. This also caches the bin-break computation, which is the expensive half.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Comma-separated list of tagged concept references. See
#'   \code{\link{getMeasurementValueHistogram}} for the tag grammar.
#' @param yearsRange Optional integer vector of length 2, `c(startYear, endYear)`.
#' @param sexStratum Optional integer vector of `gender_concept_id` values to restrict to.
#' @param ageStratum Optional integer vector of `age_decile` values to restrict to.
#' @param visitStratum Optional integer vector of `visit_group_concept_id` values to restrict to.
#' @param nBins Number of bins to divide the robust range into.
#'
#' @importFrom memoise memoise
#'
#' @return Same shape as \code{\link{getMeasurementValueHistogram}}.
#'
#' @export
getMeasurementValueHistogram_memoise <- memoise::memoise(
    getMeasurementValueHistogram,
    omit_args = "CDMdbHandler"
)
