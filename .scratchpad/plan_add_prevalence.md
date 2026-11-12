# Add `/getPersonCountsPrevalence` — per-year prevalence for tagged concept sets

## Context

Today the person-level bridge (`stratified_persons`) backs two getters, both of
which answer *"how many persons have these codes"*:

- `getPersonCountsFilters()` — the pooled population broken down by each filter
  dimension.
- `getPersonCountsUpset()` — exact exclusive set-overlap regions.

Neither gives a **denominator**, so a client can't turn a count into a rate. The
new endpoint adds the per-year numerator *and* the per-year population at risk,
so the client can plot `person_counts / observed_persons_counts * 100`.

**Same inputs as `/getPersonCountsUpset`** (`conceptIds`, `yearsRange`,
`sexStratum`, `ageStratum`, `visitStratum`), different output: one row per
(tagged token × calendar year).

| Column | Meaning |
|--------|---------|
| `tagged_concept_id` | the tagged token from the input (`"317009SD"`), one row per token |
| `calendar_year` | the year |
| `person_counts` | distinct persons matching that token's id set **and** the requested strata **and** that year |
| `observed_persons_counts` | distinct persons under observation in that year matching the requested strata (no concept restriction) |

---

## DECISION 1 — what `observed_persons_counts` means — **RESOLVED: Option A**

The denominator must be **precomputed** (it is identical for every concept list,
so recomputing it per request is pure waste) and it must stay **exact under
filtering**. Exactness is the constraint that decides the table's grain:

> Distinct-person counts can only be summed across a dimension that *partitions*
> persons. Within one calendar year a person has exactly one `gender_concept_id`
> and exactly one `age_decile` — so `year × sex × age` is a partition and
> `SUM(person_counts)` over selected cells is exact. **`visit_group_concept_id`
> is not a partition**: the same person can have inpatient *and* outpatient
> visits in the same year, so summing over selected visit groups double-counts.

### Option A — population at risk, visit-agnostic ✅ **chosen**

Precomputed table at grain `calendar_year × gender_concept_id × age_decile`,
built from `person ⨝ observation_period` (the Achilles analysis-116 definition
already sketched in the dead `inst/sql/*/createObservationCountsTable.sql`).

- Tiny (~#years × 2 × ~12 ≈ 1k rows) — ships in the SQLite fixture for free,
  nothing to cap or subset.
- Exact under `sexStratum` / `ageStratum` / `yearsRange`.
- **`visitStratum` is not applied to the denominator** and must be documented as
  such in the roxygen, the endpoint docs and `STRUCTURE.md`.
- Epidemiologically this is the standard denominator: the ratio reads as
  *"prevalence of <concept> recorded through <visit group>, in the whole observed
  population"*. The numerator restriction narrows what counts as a case; the
  population at risk does not change because of how a case was recorded.

### Option B — denominator also conditioned on visit group ❌ **not chosen**

"Observed" becomes *"had a visit of group G in year Y"*. Exactness then forces a
**person-level** denominator table (`person_id, calendar_year,
gender_concept_id, age_decile, visit_group_concept_id`) — an aggregate table with
a `visit_group_concept_id` column would be wrong the moment more than one group
is selected.

- Correct for the reading *"among persons seen in this setting, what fraction had
  the code"*.
- Cost: on FinnGen roughly persons × years × groups rows (tens of millions) — a
  new large build artifact, a new capping strategy for the fixture, and a
  `COUNT(DISTINCT)` at request time instead of a `SUM`.

The rest of this plan is written for Option A.

## DECISION 2 — output column names — **RESOLVED: `tagged_concept_id`, `calendar_year`**

The request wrote `conceptId, year, person_counts, observed_persons_counts`. The
repo convention is snake_case everywhere, and the value in the first column is a
*tagged token* (`"317009SD"`), not an integer concept id — calling it
`concept_id` would be type-confusing next to the integer `concept_id` columns in
every other getter. Chosen: **`tagged_concept_id`**, **`calendar_year`**,
`person_counts`, `observed_persons_counts`.

---

## Part 1 — New precomputed table `observed_persons_counts_stratified` (BUILD)

**SQL:** `inst/sql/sql_server/createObservedPersonsCountsTable.sql` (canonical;
add a `bigquery/` override only if translation fails — it should not).

```sql
DROP TABLE IF EXISTS @resultsDatabaseSchema.@observedPersonsCountsTable;

CREATE TABLE @resultsDatabaseSchema.@observedPersonsCountsTable (
    calendar_year INTEGER,
    gender_concept_id INTEGER,
    age_decile INTEGER,
    observed_persons_counts INTEGER
);

INSERT INTO @resultsDatabaseSchema.@observedPersonsCountsTable
SELECT
    y.calendar_year            AS calendar_year,
    p.gender_concept_id        AS gender_concept_id,
    FLOOR((y.calendar_year - p.year_of_birth) / 10) AS age_decile,
    COUNT_BIG(DISTINCT p.person_id) AS observed_persons_counts
FROM (
    SELECT DISTINCT calendar_year
    FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable
) y
CROSS JOIN @cdmDatabaseSchema.person p
INNER JOIN @cdmDatabaseSchema.observation_period op
    ON p.person_id = op.person_id
WHERE YEAR(op.observation_period_start_date) <= y.calendar_year
  AND YEAR(op.observation_period_end_date)   >= y.calendar_year
GROUP BY y.calendar_year, p.gender_concept_id,
         FLOOR((y.calendar_year - p.year_of_birth) / 10);
```

Two deliberate choices:

- **The year list comes from `stratified_code_counts`, not a recursive CTE.** The
  dead `createObservationCountsTable.sql` uses `WITH RECURSIVE` to expand
  min→max observation year; that is not SQL Server dialect (SqlRender's source
  dialect) and recursive CTEs are the kind of thing that breaks on one backend
  and not the other. Reading the distinct years out of the already-built
  stratified table is dialect-neutral, and it bounds the denominator to exactly
  the years the API can ever show — a stray `observation_period_end_date` of
  2999 can't blow the table up. **Consequence: the new table must be built
  *after* `stratified_code_counts`.**
- `age_decile` uses `FLOOR((year - year_of_birth) / 10)`, byte-for-byte the
  expression in `appendToStratifiedPersonsTable.sql`, so numerator and
  denominator deciles line up. Persons with a NULL `year_of_birth` land in a
  NULL decile on both sides.

**R:** `R/createObservedPersonsCountsTable.R` →
`createObservedPersonsCountsTable(CDMdbHandler, observedPersonsCountsTable =
"observed_persons_counts_stratified", stratifiedCodeCountsTable =
"stratified_code_counts")`, exported, same read-SQL / render / translate /
`executeSql` shape as `createStratifiedPersonsTable()`.

**Wire into the build:** call it from `createCodeCountsTables()`
(`R/createCodeCountsTables.R`) after `createStratifiedCodeCountsTable()` and
`createStratifiedPersonsTable()`, before the `code_counts` rollup. Nothing in
`code_counts` depends on it; ordering only matters w.r.t. the stratified table.

**Dead code to remove in the same change:** `inst/sql/sql_server/`
and `inst/sql/bigquery/createObservationCountsTable.sql` plus the commented-out
`createObservationCountsTable` block in `tests/testthat/test-createCodeCountsTable.R`
(lines ~287-313). Nothing calls them; leaving a near-duplicate
`observation_counts` next to the new table is a trap.

## Part 2 — Getter `getPersonCountsPrevalence()`

New file `R/getPersonCountsPrevalence.R`, modelled on `getPersonCountsUpset.R`.

Signature identical to `getPersonCountsUpset()`:

```r
getPersonCountsPrevalence(CDMdbHandler, conceptIds, yearsRange = NULL,
                          sexStratum = NULL, ageStratum = NULL, visitStratum = NULL)
```

Validation: copied verbatim from `getPersonCountsUpset()` (checkmate asserts +
the `yearsRange[1] > yearsRange[2]` stop).

Body:

1. `.parsePersonCountsConceptIds()` → `.resolveTaggedConceptIdSets()` — reuse,
   unchanged.
2. **Numerator** — one `SELECT` per token `UNION ALL`-ed, exactly like the upset
   getter builds `tokenQueries`, but grouped by year instead of pulling raw
   person rows:

   ```sql
   SELECT '<token>' AS tagged_concept_id, calendar_year,
          COUNT(DISTINCT person_id) AS person_counts
   FROM @resultsDatabaseSchema.@stratifiedPersonsTable
   WHERE <column> IN (<ids>) <strata filters>
   GROUP BY calendar_year
   ```

   Strata filters via the existing `.inFilterSql()` / `.betweenFilterSql()`
   helpers — all four dimensions apply here, including `visitStratum`.
3. **Denominator** — one query against the new table, `SUM` (not
   `COUNT(DISTINCT)`; the grain already guarantees one row per person per
   year/sex/age cell):

   ```sql
   SELECT calendar_year, SUM(observed_persons_counts) AS observed_persons_counts
   FROM @resultsDatabaseSchema.@observedPersonsCountsTable
   WHERE 1=1 <sex filter> <age filter> <year filter>
   GROUP BY calendar_year
   ```

   `visitStratum` is deliberately absent (DECISION 1).
4. **Join in R**: complete the grid `tokens × denominator years` so every token
   has a dot in every year the population exists (missing numerator → `0`),
   then `dplyr::left_join()` the denominator. Drop years whose denominator is
   `0`/`NA` — a prevalence can't be formed there and an `Inf` in the JSON helps
   nobody. Return
   `tagged_concept_id, calendar_year, person_counts, observed_persons_counts`,
   arranged by token then year.

Add `getPersonCountsPrevalence_memoise <- memoise::memoise(...,
omit_args = "CDMdbHandler")` in the same file, mirroring the others.

> Note: the function returns **counts, not a rate**. Division is the client's
> (and the report plot's) job — consistent with every other getter returning raw
> counts, and it keeps the two numbers auditable.

## Part 3 — Endpoint `/getPersonCountsPrevalence`

In `inst/plumber/plumber.R`, a copy of the `/getPersonCountsUpset` block: same
`conceptIds` empty-check, same `.plumberParseYearsRange()` 400, same
`.plumberParseIntCsv()` for the three strata, same `tryCatch` → 400. Delegates to
`getPersonCountsPrevalence_memoise`. Roxygen-style `#*` docs must state that
`visitStratum` narrows the numerator only.

## Part 4 — Fixtures

- `helper_createSqliteDatabaseFromDatabase()` (`R/helper.R`): extract the new
  table **verbatim** (`SELECT * FROM @resultsDatabaseSchema.observed_persons_counts_stratified`)
  — it is ~1k rows, no concept scoping and no person cap apply.
- `inst/testdata/data/createTestingData.R`: add `"observed_persons_counts_stratified"`
  to the `expect_equal` table list and a `count() > 0` check.
- **Regenerate `inst/testdata/data/FinnGenR13_countsOnly.sqlite`** by running
  that script — needs BigQuery access to `AtlasDevelopment-full`, so this is a
  human step. Until it is run, the new post-counts tests fail against
  OnlyCounts-FinnGen (same situation as the `stratified_persons` rollout; the
  existing note at the top of `test-getPersonCountsUpset.R` is the precedent).
- `development/sandbox/export_precomputed_tables.sh`: add the table to the
  `TABLES=(...)` array.

> **Fixture caveat to write into the test file header:** the fixture's
> `stratified_persons` is a *capped sample* (`maxPersonsPerConcept`), while the
> new denominator is extracted uncapped from the full population. Prevalence
> *values* computed from the fixture are therefore meaningless — only shape,
> bounds (`person_counts <= observed_persons_counts` is **not** guaranteed to be
> tight but must hold) and invariants are testable there. Exact arithmetic is
> covered by the synthetic fixture and by the creation-stage databases.

## Part 5 — Tests

**`tests/testthat/test-getPersonCountsPrevalence.R`** (post-counts stage:
OnlyCounts-FinnGen + AtlasDevelopment-5k):

- rejects malformed tokens (`""`, `"abc"`, `"317009X"`, `"317009"`) and an
  inverted `yearsRange` — mirrors the upset tests.
- column names / no-NA / `nrow > 0` for `"317009SD"`.
- `yearsRange` restricts the returned `calendar_year` range.
- cross-check against the existing getter: summing `person_counts` over years is
  **≥** the `getPersonCountsFilters()` total for the same input (a person active
  in two years is counted twice here, once there) and each year's
  `person_counts` equals the matching `filter == "year"` row of
  `getPersonCountsFilters()` for a single pooled token.
- `person_counts <= observed_persons_counts` for every row.

**Synthetic exact test** — extend `.buildSyntheticPersonCountsHandler()`
(`tests/testthat/helper.R`) to also insert an
`observed_persons_counts_stratified` table with hand-chosen round numbers (e.g.
1000 observed persons per year/sex/age cell) so the expected prevalence is
computable by hand. The existing 8-row `stratified_persons` fixture then gives,
for `"100SD,200MD"`, a small exact table (persons 1,2,4 in 2010; 3,6 in 2011;
5 in 2012 — recompute precisely when writing the test). Adding a table to the
helper cannot affect the existing upset/filters tests.

**`tests/testthat/test-createCodeCountsTable.R`** (creation stage:
Eunomia-GiBleed + AtlasDevelopment-5k): a `createObservedPersonsCountsTable
works` test — table exists, non-empty, no NA in `calendar_year`, all
`observed_persons_counts > 0`, and the set of `calendar_year` values equals the
distinct years in `stratified_code_counts`.

**`development/TEST_SUMMARY.md`**: add the new file to the catalogue.

## Part 6 — Report visualisation

`createPrevalencePlotFromPersonCounts(prevalencePersonCounts, concepts = NULL)`
in `R/plotingFunctions.R` — a clustered dot chart:

- x = `calendar_year`, y = `person_counts / observed_persons_counts * 100`,
  one colour + one series per `tagged_concept_id`.
- labels follow `createUpsetPlotFromPersonCounts()`'s convention: strip the
  leading digits with `stringr::str_extract(token, "^[0-9]+")`, look the name up
  in `concepts`, and render `"<concept_name> (<token>)"` so tag variants stay
  distinguishable.
- `plotly::plot_ly(..., type = "scatter", mode = "markers+lines")` (markers are
  what was asked for; the connecting line makes a time series readable — drop it
  if unwanted), y-axis titled `"Prevalence (%)"`, hover showing both raw counts.

`inst/reports/testReport.Rmd`: in the existing *Person Counts* section, add
`prevalence_person_counts = getPersonCountsPrevalence_memoise(CDMdbHandler,
conceptIds = paste0(conceptId, "SD"))` to the `personCounts` list and a new
`Prevalence` chunk calling the plot. (The report only ever passes one token, so
the chart shows a single series — the function must still handle N.)

`createReport()`'s roxygen `@details` lists the report's contents; add the
prevalence chart there.

## Part 7 — Docs

- `devtools::document()` after the roxygen is in (new exported functions:
  `createObservedPersonsCountsTable`, `getPersonCountsPrevalence`,
  `getPersonCountsPrevalence_memoise`, `createPrevalencePlotFromPersonCounts`).
- `development/STRUCTURE.md`:
  - §2 — a new `2d · observed_persons_counts_stratified` subsection (grain, why
    `year × sex × age` is additive and visit group is not), plus the mermaid
    build block.
  - §3 — a row in the getters table and an *Output shapes* subsection for
    `getPersonCountsPrevalence()`.
  - §3 *Reports & plots* — the new chart.
  - §4 — a row in the endpoints table.
- `NEWS.md` — one bullet.

## Verification

1. `devtools::document()` — clean.
2. `HADESEXTAS_TESTING_ENVIRONMENT=Eunomia-GiBleed BUILD_COUNTS_TABLE=TRUE
   devtools::test()` — creation stage (builds the new table from a raw CDM).
3. Regenerate the FinnGen fixture (human, needs BigQuery), then
   `HADESEXTAS_TESTING_ENVIRONMENT=OnlyCounts-FinnGen devtools::test()`.
4. `HADESEXTAS_TESTING_ENVIRONMENT=AtlasDevelopment-5k devtools::test()` — both
   stages on BigQuery, confirms the SQL translates.
5. `devtools::check()` — new exports touch `NAMESPACE`/`man`.
6. Manual smoke: `runApiServer(buildCountsTable = TRUE)` on Eunomia, then
   `GET /getPersonCountsPrevalence?conceptIds=317009SD` and
   `GET /report?conceptId=317009` (the prevalence chart renders).

## Notes / caveats

- **Rebuild required on deploy.** This adds a precomputed table, so every
  existing deployment needs one `runApiServer(..., buildCountsTable = TRUE)`
  pass before `/getPersonCountsPrevalence` works. The endpoint should fail with
  a clear 400 ("table not found") rather than a stack trace — the existing
  `tryCatch` in the plumber block already does that.
- **Numerator semantics are "person had ≥1 record of the code in year Y"**, not
  "person had the condition in year Y". For chronic conditions this reads as
  *recorded* prevalence and will dip in years a patient simply wasn't seen. That
  is the only thing `stratified_persons` can answer; worth stating in the
  roxygen so nobody reads the chart as true disease prevalence.
- Prevalence of a **non-standard/mapped token** (`M`) is bounded by how
  completely that source vocabulary is used in the database; comparing an `M`
  series against an `S` series on the same chart compares coding practice as
  much as disease frequency.
- Possible follow-up (out of scope): age-standardised rates, which would need the
  denominator broken out by decile at request time rather than summed.
