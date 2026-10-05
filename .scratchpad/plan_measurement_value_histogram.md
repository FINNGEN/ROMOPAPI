# `getMeasurementValueHistogram` — build notes

Status: **implemented**. Record of what was built and the findings worth keeping.
See `development/STRUCTURE.md` §2b-bis, §3, §4 for the living documentation.

## What it does

`GET /getMeasurementValueHistogram` — same inputs as `/getPersonCountsUpset`
(`conceptIds`, `yearsRange`, `sexStratum`, `ageStratum`, `visitStratum`) plus
`nBins` (default 100). Measurement-domain concepts only; other domains error.

| Column | Meaning |
|--------|---------|
| `tagged_conceptid` | the entry of `conceptIds` these events came from |
| `measured_value_bucket` | right-closed `(x1, x2]` label |
| `unit` | `concept_code` of the unit; `NA` if none recorded |
| `n_events` | events in that bucket, for that entry |

## The two rules that define the behaviour

1. **Bins are computed per *unit*, pooled across every `conceptIds` entry sharing
   that unit.** Same unit → one histogram on shared buckets, one series per
   `tagged_conceptid`, stackable as coloured segments of one bar. Different units
   → separate histograms (values aren't comparable).
2. **Breaks come from the *unfiltered* events, counts from the *filtered* ones.**
   So the buckets stay put as a client moves the sex/age/visit/year filters.

Range is robust — `median ± 7·MAD`, not min/max — so one outlier can't collapse
everything into one bin. Out-of-range values land in an underflow `(-Inf, x]` and
overflow `(x, +Inf]` bin, so no event is lost. MAD of 0 (identical values) widens
the range by `soft_min_step = 0.01`. All `nBins + 2` bins returned, zero-filled.

Caveat: pooling breaks across entries means an event matched by two overlapping
entries is weighted twice in the median/MAD. Exact for the usual disjoint case.

## Files

- `R/createStratifiedMeasurementsTable.R` + `inst/sql/sql_server/appendToStratifiedMeasurementsTable.sql`
  — build-phase table, one row per measurement event with a numeric value
  (deliberately not de-duplicated). Hooked into `createCodeCountsTables()`.
- `R/getMeasurementValueHistogram.R` — getter + memoised twin. All median/MAD/binning
  happens in SQL; R only labels buckets and resolves the unit's `concept_code`.
- `R/sqlFilterHelpers.R` — `.inFilterSql`/`.betweenFilterSql`, previously one per
  file in `getPersonCountsUpset.R`/`getPersonCountsFilters.R` (not duplicated, just
  split), now shared by three callers.
- `inst/plumber/plumber.R` — the endpoint.
- `R/plotingFunctions.R::createMeasurementHistogramPlot()` + `inst/reports/testReport.Rmd`
  — a **Measured Values** section in `/report`, bars stacked by `tagged_conceptid`,
  one facet per unit. Rendered only for Measurement concepts that have values, since
  the getter errors on other domains.

## Findings worth remembering

- **⚠ SqlRender mistranslates a `GROUP BY` containing an expression.**
  `GROUP BY a, b, CASE…END` became `group by 3, 2, 3` for BigQuery — silently
  dropping a grouping column, wrong counts in production, correct on SQLite. Fix:
  materialise the expression in its own CTE so `GROUP BY` lists only plain column
  names. **Never put an expression in a `GROUP BY` in this codebase.** (Hit twice.)
- **No `PERCENTILE_CONT` needed.** SQLite has no median at all and BigQuery spells
  it differently. `ROW_NUMBER()`/`COUNT() OVER` + `AVG` of the middle row(s) is
  portable and translates unchanged. Verified against an R re-implementation of the
  reference `binning_soft_mad`: 25/25 bins and 485/485 events matched exactly.
- **`FLOOR` returns a float** — bin index needs an explicit `CAST(… AS INT)`, or it
  comes back as `2.0`. SqlRender renders that to `int64` for BigQuery correctly.
- **Fixed 2 dp bucket labels are unsafe.** A low-dispersion partition (range only
  `2 × soft_min_step` wide) rendered several distinct bins as the identical string
  `(42.00, 42.00]`, breaking any client keying a chart on the label. `.labelDigits()`
  widens precision when the bin width needs it. Regression test covers it.
- **Eunomia-GiBleed has 44,053 measurement rows and *zero* `value_as_number`.** The
  table is legitimately empty there. The creation test therefore asserts a
  relationship to the source (`built <= source_with_values`, non-empty only if the
  CDM has values) rather than `n > 0`.
- **Fixture sampling must preserve the distribution.** The extract caps at
  `maxEventsPerConcept` (2000) per (concept, unit) via an `NTILE` systematic sample
  over value order. Taking the first N by value would have kept only the smallest
  readings and made the histogram tests meaningless.
- **Unit concepts must be extracted into the fixture.** The concept extraction
  originally pulled only tree + visit-group concepts, so the unit was missing and
  every `unit` came back `NA`. A third `UNION` branch fixes it.
- **Size was a non-issue.** FinnGen `measurement` is 0.39 GB / 2.85M rows;
  `stratified_measurements` built in ~9 s (872,390 rows). Only that one table was
  built on BigQuery — the other three are concept-agnostic and already existed.

## Testing

- `test-createStratifiedMeasurementsTable.R` — creation stage (Eunomia-GiBleed,
  AtlasDevelopment-5k).
- `test-getMeasurementValueHistogram.R` — exact arithmetic against a hand-built
  synthetic fixture (`.buildSyntheticMeasurementHandler()` in `helper.R`, ids
  `9000001`/`9000002`, deliberately fake so they collide with nothing), plus
  post-counts tests against the shipped FinnGen fixture.
- Fixture concept is **`40652733`** — LOINC Group "C reactive protein", no events of
  its own, two descendants `3020460` (serum/plasma) and `3051387` (capillary) **both
  in mg/L**. So `40652733S` returns nothing, `40652733SD` pools both into one series,
  and `3020460S,3051387S` gives two stackable series on shared buckets — the case the
  endpoint exists to support.

## Out of scope

- **Privacy floor.** The reference's `keep_green` drops bins below a person count.
  Not wanted, and not free: per-bin distinct-person counts can't be summed across
  strata, so it would need a second person-level bridge table.
- **Unit harmonization** and the reference's `discrete_units` exclusion — no
  harmonization layer exists, so partitioning is on raw `unit_concept_id`.
- **Observation domain**, which also has `value_as_number`. The build template is
  parameterised like its siblings, so adding it is a one-row domain-table change.
