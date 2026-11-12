# These tests run against a hand-built synthetic fixture
# (.buildSyntheticMeasurementHandler() in helper.R) rather than a real CDM, so the
# soft-MAD bin arithmetic can be asserted exactly. See that helper for the dataset
# and the hand-computed breaks.

test_that("getMeasurementValueHistogram rejects malformed conceptIds tokens", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  expect_error(getMeasurementValueHistogram(CDMdbHandler, conceptIds = ""))
  expect_error(getMeasurementValueHistogram(CDMdbHandler, conceptIds = "abc"))
  expect_error(getMeasurementValueHistogram(CDMdbHandler, conceptIds = "9000001X"))
  expect_error(getMeasurementValueHistogram(CDMdbHandler, conceptIds = "9000001"))
})

test_that("getMeasurementValueHistogram rejects non-Measurement concepts", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  # 317009 is a Condition in the fixture
  expect_error(
    getMeasurementValueHistogram(CDMdbHandler, conceptIds = "317009S"),
    "Measurement"
  )
  # an unknown concept is an error too, rather than a silently empty histogram
  expect_error(
    getMeasurementValueHistogram(CDMdbHandler, conceptIds = "99999999S"),
    "not found"
  )
})

test_that("getMeasurementValueHistogram returns the documented shape", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  histogram <- getMeasurementValueHistogram(CDMdbHandler, conceptIds = "9000001S", nBins = 4L)

  histogram |>
    colnames() |>
    expect_equal(c("tagged_conceptid", "measured_value_bucket", "unit", "n_events"))

  histogram |>
    dplyr::pull(tagged_conceptid) |>
    unique() |>
    expect_equal("9000001S")

  # unit resolves to the concept_code, not the raw id
  histogram |>
    dplyr::pull(unit) |>
    unique() |>
    expect_equal("mg/dL")

  # all nBins + 2 bins are returned, zero-filled
  histogram |>
    nrow() |>
    expect_equal(6)

  histogram |>
    dplyr::filter(is.na(measured_value_bucket) | is.na(n_events)) |>
    nrow() |>
    expect_equal(0)
})

test_that("getMeasurementValueHistogram bins on the robust MAD range, not min/max", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  # The mg/dL partition holds all 16 values: 1..11, the outliers -100 and 500, and
  # the three 2020 rows (5, 6, 7). Sorted, the median is 6; the absolute deviations
  # from it have median 2.5, so MAD = 2.5 and the robust range is 6 +/- 7*2.5 =
  # [-11.5, 23.5]. With nBins = 4 the in-range width is 35/4 = 8.75, giving bins
  # (-11.5, -2.75], (-2.75, 6], (6, 14.75], (14.75, 23.5] plus the two outer ones.
  histogram <- getMeasurementValueHistogram(CDMdbHandler, conceptIds = "9000001S", nBins = 4L) |>
    dplyr::filter(unit == "mg/dL")

  buckets <- histogram |> dplyr::pull(measured_value_bucket)

  # the outliers must NOT stretch the in-range edges -- that is the point of using
  # MAD rather than min/max, which would have given a range of [-100, 500]
  expect_true("(-Inf, -11.50]" %in% buckets)
  expect_true("(23.50, +Inf]" %in% buckets)

  counts <- stats::setNames(histogram$n_events, histogram$measured_value_bucket)

  # -100 underflows, 500 overflows
  expect_equal(unname(counts[["(-Inf, -11.50]"]]), 1L)
  expect_equal(unname(counts[["(23.50, +Inf]"]]), 1L)

  # right-closed bins: 1,2,3,4,5,5,6,6 -> (-2.75, 6]; 7,7,8,9,10,11 -> (6, 14.75]
  expect_equal(unname(counts[["(-2.75, 6.00]"]]), 8L)
  expect_equal(unname(counts[["(6.00, 14.75]"]]), 6L)

  # nothing in the remaining in-range bins
  expect_equal(unname(counts[["(-11.50, -2.75]"]]), 0L)
  expect_equal(unname(counts[["(14.75, 23.50]"]]), 0L)

  # every event is accounted for exactly once
  expect_equal(sum(histogram$n_events), 16L)
})

test_that("getMeasurementValueHistogram splits a D token into one histogram per unit", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  # 9000001SD expands to itself (mg/dL) and 9000002 (mmol/L and mg/dL)
  histogram <- getMeasurementValueHistogram(CDMdbHandler, conceptIds = "9000001SD", nBins = 4L)

  histogram |>
    dplyr::pull(unit) |>
    unique() |>
    sort() |>
    expect_equal(c("mg/dL", "mmol/L"))

  # each unit is its own histogram, so each gets its own full set of bins
  histogram |>
    dplyr::count(unit) |>
    dplyr::pull(n) |>
    expect_equal(c(6L, 6L))

  # the mmol/L partition holds only the descendant's 5 values
  histogram |>
    dplyr::filter(unit == "mmol/L") |>
    dplyr::pull(n_events) |>
    sum() |>
    expect_equal(5L)
})

test_that("getMeasurementValueHistogram shares buckets across sets of the SAME unit", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  # two separate sets, both recorded in mg/dL: the breaks are pooled per unit, so the
  # two series must come back on identical buckets -- that is what lets a client stack
  # them as coloured segments of one bar
  histogram <- getMeasurementValueHistogram(
    CDMdbHandler,
    conceptIds = "9000001S,9000002S", nBins = 4L
  ) |>
    dplyr::filter(unit == "mg/dL")

  histogram |>
    dplyr::pull(tagged_conceptid) |>
    unique() |>
    sort() |>
    expect_equal(c("9000001S", "9000002S"))

  byToken <- histogram |>
    split(~tagged_conceptid) |>
    lapply(\(x) as.character(x$measured_value_bucket))

  expect_equal(byToken[["9000001S"]], byToken[["9000002S"]])

  # every bucket appears exactly once per set, so the stack is well formed
  histogram |>
    dplyr::count(measured_value_bucket) |>
    dplyr::pull(n) |>
    unique() |>
    expect_equal(2L)

  # 16 mg/dL events on the first set, 6 on the second, none lost or double counted
  expect_equal(sum(histogram$n_events), 22L)
})

test_that("getMeasurementValueHistogram keeps sets of DIFFERENT units apart", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  # 9000001 is mg/dL only; 9000002 also has mmol/L. Different units are not
  # comparable, so they must not share a bucket scale.
  histogram <- getMeasurementValueHistogram(
    CDMdbHandler,
    conceptIds = "9000001S,9000002S", nBins = 4L
  )

  buckets <- histogram |>
    split(~unit) |>
    lapply(\(x) unique(as.character(x$measured_value_bucket)))

  expect_false(identical(buckets[["mg/dL"]], buckets[["mmol/L"]]))
})

test_that("getMeasurementValueHistogram handles a zero-MAD partition", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  # 9000002 in mg/dL is six identical values (42) -> MAD 0 -> soft-step range
  histogram <- getMeasurementValueHistogram(CDMdbHandler, conceptIds = "9000002S", nBins = 4L) |>
    dplyr::filter(unit == "mg/dL")

  # all six events survive, in a single populated bin
  histogram |>
    dplyr::pull(n_events) |>
    sum() |>
    expect_equal(6L)

  histogram |>
    dplyr::filter(n_events > 0) |>
    nrow() |>
    expect_equal(1)

  # Regression: the soft-step range here is only 0.02 wide, so bin edges differ in
  # the 3rd decimal. Formatting labels at a fixed 2 decimals collapsed several
  # distinct bins onto the identical string "(42.00, 42.00]", which breaks any
  # client keying its chart on the bucket label.
  histogram |>
    dplyr::pull(measured_value_bucket) |>
    anyDuplicated() |>
    expect_equal(0)
})

test_that("getMeasurementValueHistogram bucket labels are unique within a partition", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  getMeasurementValueHistogram(CDMdbHandler, conceptIds = "9000001SD", nBins = 4L) |>
    dplyr::count(unit, measured_value_bucket) |>
    dplyr::pull(n) |>
    unique() |>
    expect_equal(1L)
})

test_that("getMeasurementValueHistogram filters change counts but NOT bucket edges", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  unfiltered <- getMeasurementValueHistogram(
    CDMdbHandler,
    conceptIds = "9000001S", nBins = 4L
  ) |> dplyr::filter(unit == "mg/dL")

  filtered <- getMeasurementValueHistogram(
    CDMdbHandler,
    conceptIds = "9000001S", nBins = 4L, sexStratum = 8532L
  ) |> dplyr::filter(unit == "mg/dL")

  # the breaks are derived from the UNFILTERED events, so the buckets are identical
  expect_equal(
    filtered |> dplyr::pull(measured_value_bucket),
    unfiltered |> dplyr::pull(measured_value_bucket)
  )

  # but only the three 2020/female rows (5, 6, 7) are counted
  expect_equal(sum(filtered$n_events), 3L)
  expect_lt(sum(filtered$n_events), sum(unfiltered$n_events))
})

test_that("getMeasurementValueHistogram applies year and age filters", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  byYear <- getMeasurementValueHistogram(
    CDMdbHandler,
    conceptIds = "9000001S", nBins = 4L, yearsRange = c(2020L, 2020L)
  ) |> dplyr::filter(unit == "mg/dL")

  expect_equal(sum(byYear$n_events), 3L)

  # the fixture puts everything in age decile 5
  byAge <- getMeasurementValueHistogram(
    CDMdbHandler,
    conceptIds = "9000001S", nBins = 4L, ageStratum = 9L
  ) |> dplyr::filter(unit == "mg/dL")

  expect_equal(sum(byAge$n_events), 0L)
})

#
# Against the shipped FinnGen fixture. Concept 40652733 is the LOINC *Group*
# "C reactive protein | Mass Concentration | Blood, Serum or Plasma" — a classification
# concept with NO events of its own. Its data sits on TWO descendants, 3020460
# (Serum or Plasma) and 3051387 (Capillary blood), BOTH recorded in mg/L. So it only
# yields a histogram under a D token, and because the two children share a unit they
# pool onto one set of buckets — the stackable case.
#

test_that("getMeasurementValueHistogram works on the FinnGen fixture", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  memoise::forget(getMeasurementValueHistogram_memoise)

  histogram <- getMeasurementValueHistogram(CDMdbHandler, conceptIds = "40652733SD", nBins = 10L)

  histogram |>
    colnames() |>
    expect_equal(c("tagged_conceptid", "measured_value_bucket", "unit", "n_events"))

  # all nBins + 2 bins, and the unit resolves to the creatinine unit
  histogram |>
    nrow() |>
    expect_equal(12)

  histogram |>
    dplyr::pull(unit) |>
    unique() |>
    expect_equal("mg/L")

  histogram |>
    dplyr::pull(n_events) |>
    sum() |>
    expect_gt(0)

  # labels must stay unique, or a client keying a chart on them breaks
  histogram |>
    dplyr::pull(measured_value_bucket) |>
    anyDuplicated() |>
    expect_equal(0)

  # the robust range must not be stretched by the long upper tail: real serum
  # most CRP readings are low (single digits to tens of mg/L) with a long right
  # tail, so the modal bin must be well below the maximum observed value
  modalBucket <- histogram |>
    dplyr::slice_max(n_events, n = 1) |>
    dplyr::pull(measured_value_bucket)
  expect_false(grepl("Inf", modalBucket))
})

test_that("getMeasurementValueHistogram on the fixture: a grouper alone has no events", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  # 40652733 is a LOINC Group: it has descendants with data but no events of its own,
  # so without the D tag there is nothing to histogram
  getMeasurementValueHistogram(CDMdbHandler, conceptIds = "40652733S", nBins = 10L) |>
    nrow() |>
    expect_equal(0)
})

test_that("getMeasurementValueHistogram on the fixture: filters never move the buckets", {
  skip_if_not(testingDatabase %in% postCountsDatabases)

  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  unfiltered <- getMeasurementValueHistogram(CDMdbHandler, conceptIds = "40652733SD", nBins = 10L)
  filtered <- getMeasurementValueHistogram(
    CDMdbHandler,
    conceptIds = "40652733SD", nBins = 10L, sexStratum = 8532L
  )

  expect_equal(
    filtered |> dplyr::pull(measured_value_bucket),
    unfiltered |> dplyr::pull(measured_value_bucket)
  )
  expect_lt(sum(filtered$n_events), sum(unfiltered$n_events))
  expect_gt(sum(filtered$n_events), 0)
})

test_that("getMeasurementValueHistogram rejects an invalid yearsRange and nBins", {
  CDMdbHandler <- .buildSyntheticMeasurementHandler()

  expect_error(
    getMeasurementValueHistogram(CDMdbHandler, conceptIds = "9000001S", yearsRange = c(2020L, 2010L)),
    "first year must be"
  )
  expect_error(
    getMeasurementValueHistogram(CDMdbHandler, conceptIds = "9000001S", nBins = 0L)
  )
})
