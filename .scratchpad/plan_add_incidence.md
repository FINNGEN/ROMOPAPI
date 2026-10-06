# Add `/getPersonCountsIncidence` + `/getPersonCountsIncidenceFilters`

## Context

Today `stratified_persons` backs three getters, all counting *any* occurrence
of a tagged concept set in a stratum/year:

- `getPersonCountsFilters()` — pooled population, broken down by dimension.
- `getPersonCountsUpset()` — exact exclusive set-overlap regions.
- `getPersonCountsPrevalence()` — per-year numerator + population-at-risk
  denominator.

This request adds the **incidence** counterpart of the last two: count a
person only once, in the year of their **first-ever** record of the tagged
concept set (the concept itself or any of its descendants) — not every year
they happen to have a record.

Two new functions/endpoints, same input shape as their prevalence/filters
siblings:

- **`getPersonCountsIncidence(conceptIds, yearsRange, sexStratum, ageStratum,
  visitStratum)`** → same output shape as `getPersonCountsPrevalence()`:
  `tagged_concept_id, calendar_year, person_counts, observed_persons_counts`,
  but `person_counts` here counts only persons whose first-ever record of that
  token's id set falls in that year (and qualifies under the requested
  strata).
- **`getPersonCountsIncidenceFilters(conceptIds, yearsRange, sexStratum,
  ageStratum, visitStratum)`** → same output shape as
  `getPersonCountsFilters()`: `filter, stratum, person_counts, selected`, but
  pooling only each person's first-ever incident event per set in
  `conceptIds`, instead of every occurrence.

---

## DECISION 1 — incidence denominator — **RESOLVED: reuse the prevalence table as-is**

Reuse `observed_persons_counts_stratified` unchanged (population under
observation that year, `sexStratum`/`ageStratum`/`yearsRange`-exact,
`visitStratum` not applied — identical caveat to `getPersonCountsPrevalence()`).
No new build artifact.

Rejected alternative: subtracting, per token and year, persons already
incident in an earlier year (a true epidemiological at-risk denominator).
Correct, but needs a live per-request running-exclusion computed from
`stratified_persons` with no bound on how far back it must look — materially
more complex, and not what was asked ("very similar to prevalence"). Flagged
as a possible follow-up in Notes.

## DECISION 2 — strata vs "first" — **RESOLVED: "first" is absolute, filters apply after**

A person's first-ever record of a token's id set is computed **once, over all
history, ignoring every filter** (no `sexStratum`/`ageStratum`/`visitStratum`/
`yearsRange` in the `MIN(calendar_year)` computation). Those four filters are
then applied **only** to decide whether that already-fixed incident event
qualifies for the count — exactly how `visitStratum` already narrows the
Prevalence numerator without redefining it. Consequence: `yearsRange` can make
`getPersonCountsIncidence()` return **zero** rows for a token whose true
first-ever year falls outside the range (correct — that year's incident count
is 0 because the person's one incident year already happened, just not in the
requested window), as opposed to Prevalence where every year in range can show
a nonzero count.

Rejected alternative: letting a filter (e.g. `visitStratum`) change what
counts as "first" (first occurrence *within that visit group*). Rejected
because the answer would then depend on which filters happen to be passed,
and a chronic-condition patient's "first inpatient record" can be years after
their true first record — a materially different, and confusing, metric.

---

## Part 1 — Shared helper: per-token "incident rows"

New private helper in `R/parsePersonCountsConceptIds.R` (alongside
`.resolveTaggedConceptIdSets()`, which it consumes):

```r
.buildIncidentRowsSql(resolvedTokens, stratifiedPersonsTable = "@stratifiedPersonsTable")
```

For each resolved token, emits one `SELECT` whose rows are that token's
person-level first-occurrence events — one row per `(person_id, visit_group)`
present in the person's first incident year (a person can have more than one
visit group in their first year; keeping all of them is what lets
`visitStratum` narrow the result post-hoc, per Decision 2):

```sql
SELECT '<token>' AS tagged_concept_id, sp.person_id AS person_id,
       sp.calendar_year AS calendar_year, sp.gender_concept_id AS gender_concept_id,
       sp.age_decile AS age_decile, sp.visit_group_concept_id AS visit_group_concept_id
FROM @resultsDatabaseSchema.@stratifiedPersonsTable sp
WHERE sp.<column> IN (<ids>)
  AND sp.calendar_year = (
      SELECT MIN(calendar_year)
      FROM @resultsDatabaseSchema.@stratifiedPersonsTable
      WHERE person_id = sp.person_id AND <column> IN (<ids>)
  )
```

- A correlated subquery, not a `WITH` CTE per token — keeps the same
  "one `SELECT` per token, `UNION ALL`-joined" shape the file already uses in
  `getPersonCountsUpset()`/`getPersonCountsPrevalence()`'s `tokenQueries`, so
  this slots into both without inventing a second SQL-building idiom.
- No strata filters inside this helper — Decision 2 requires the `MIN` to see
  every row regardless of filters. Callers apply `sexStratum`/`ageStratum`/
  `visitStratum`/`yearsRange` in an outer `WHERE` after this.
- Portable: correlated subqueries with `MIN` translate identically on SQLite,
  SQL Server dialect and BigQuery — no new dialect file needed.
- Returns the `UNION ALL`-joined SQL text (no trailing `;`), so callers wrap it
  as a subquery (`FROM (<this>) incident_rows`) and append their own
  aggregation/filtering.

## Part 2 — Getter `getPersonCountsIncidence()`

New file `R/getPersonCountsIncidence.R`, modelled on
`R/getPersonCountsPrevalence.R` — same signature, same validation, same
denominator query verbatim. Only the numerator changes:

```sql
SELECT tagged_concept_id, calendar_year, COUNT(DISTINCT person_id) AS person_counts
FROM (
    <.buildIncidentRowsSql(resolvedTokens)>
) incident_rows
WHERE 1=1 <sex filter> <age filter> <year filter> <visit filter>
GROUP BY tagged_concept_id, calendar_year
```

Then the same grid-completion as Prevalence: `tidyr::expand_grid(tagged_concept_id
= resolvedTokens$token, calendar_year = denominator$calendar_year)`, left-join
numerator (coalesce missing → `0L`), left-join denominator, drop rows with no
denominator, same column order/types, same empty-result-set type coercion
guard (`as.character`/`as.integer` after each query — same SQLite bug as
Prevalence).

`getPersonCountsIncidence_memoise <- memoise::memoise(getPersonCountsIncidence,
omit_args = "CDMdbHandler")` in the same file.

## Part 3 — Getter `getPersonCountsIncidenceFilters()`

New file `R/getPersonCountsIncidenceFilters.R`, modelled on
`R/getPersonCountsFilters.R` — same signature, same validation, same
four-branch dimension-breakdown `SELECT` block and the same `selected`-column
logic, **unchanged**. Only the `tree_persons` CTE's source changes:

```sql
WITH tree_persons AS (
    SELECT DISTINCT person_id, gender_concept_id, age_decile, visit_group_concept_id, calendar_year
    FROM (
        <.buildIncidentRowsSql(resolvedTokens)>
    ) incident_rows
)
SELECT 'sex' AS filter, ... -- unchanged from getPersonCountsFilters
```

Dropping `tagged_concept_id` in the outer `SELECT DISTINCT` is what pools
across tokens (a person incident under two tokens in the same
year/sex/age/visit collapses to one row) — the same collapsing
`getPersonCountsFilters()` already relies on for raw occurrences matching more
than one requested set.

`getPersonCountsIncidenceFilters_memoise <- memoise::memoise(...)` in the same
file.

## Part 4 — Endpoints

In `inst/plumber/plumber.R`, two new blocks, each a copy of the matching
sibling's validation/`tryCatch` pattern:

- `GET /getPersonCountsIncidence?conceptIds=&yearsRange=&sexStratum=&ageStratum=&visitStratum=`
  → `getPersonCountsIncidence_memoise` (copy of the `/getPersonCountsPrevalence`
  block).
- `GET /getPersonCountsIncidenceFilters?conceptIds=&yearsRange=&sexStratum=&ageStratum=&visitStratum=`
  → `getPersonCountsIncidenceFilters_memoise` (copy of the
  `/getPersonCountsFilters` block).

Doc comments (`#*`) must state the "first occurrence is absolute, filters
apply after" semantics (Decision 2) so a client doesn't expect `yearsRange` to
shift what counts as incident.

## Part 5 — Fixtures / tests

**No new precomputed table, no fixture regeneration needed** — both getters
read only `stratified_persons` (already shipped) and
`observed_persons_counts_stratified` (already shipped as of the prevalence
PR). The existing `OnlyCounts-FinnGen` fixture and
`development/sandbox/export_precomputed_tables.sh` need no changes.

**`tests/testthat/test-getPersonCountsIncidence.R`** (post-counts stage:
OnlyCounts-FinnGen + AtlasDevelopment-5k):

- rejects malformed tokens and inverted `yearsRange` (copy of the Prevalence
  tests).
- column names / no-NA / `nrow > 0` for a real tagged token.
- for every row, `person_counts <= observed_persons_counts`.
- **invariant vs Prevalence**: for any token, `sum(person_counts)` over all
  years from Incidence is `<=` `sum(person_counts)` over all years from
  Prevalence for the same token (incidence counts each person once ever;
  prevalence can count them again in later years) — cheap cross-getter sanity
  check using the fixture, no new synthetic numbers needed.
- **narrowing `yearsRange` can drop a token to zero rows** — assert this
  explicitly for a token whose fixture-observed first year is known to sit
  outside a chosen range, to pin down Decision 2's behavior in a test (not
  just the plan doc).

**`tests/testthat/test-getPersonCountsIncidenceFilters.R`** (same stage):

- rejects malformed tokens / inverted `yearsRange` (copy of the Filters
  tests).
- `filter` has exactly the four expected values, `selected` matches the
  passed-in strata (copy of the Filters tests).
- **invariant vs Filters**: for the `"year"` rows, `sum(person_counts)` from
  IncidenceFilters is `<=` the equivalent sum from Filters (same reasoning as
  above).

**Synthetic exact tests** — extend `.buildSyntheticPersonCountsHandler()`
(`tests/testthat/helper.R`) only if the existing 8-row `stratified_persons`
fixture doesn't already give clean by-hand-computable first-occurrence years
for `"100SD,200MD"`; recompute the expected per-person first year from the
existing rows when writing the test (no new table required — only
`stratified_persons` is read, which the helper already builds). Add one
synthetic case with a person who has the **same token's concept in two
different years**, specifically to prove the second year is excluded (the
one new behavior Incidence adds over Prevalence).

**`development/TEST_SUMMARY.md`**: add both new test files to the catalogue.

## Part 6 — Report visualisation (optional, matching Prevalence's precedent)

`createIncidencePlotFromPersonCounts(incidencePersonCounts, concepts = NULL)`
in `R/plotingFunctions.R` — same clustered dot chart as
`createPrevalencePlotFromPersonCounts()` (x = `calendar_year`, y =
`person_counts / observed_persons_counts * 100`, one colour per
`tagged_concept_id`), y-axis titled `"Incidence (%)"` to distinguish it from
the Prevalence chart.

`inst/reports/testReport.Rmd`: add `incidence_person_counts =
getPersonCountsIncidence_memoise(CDMdbHandler, conceptIds = paste0(conceptId,
"SD"))` to the `personCounts` list and a new `Incidence` chunk next to the
existing `Prevalence` one.

`createReport()`'s roxygen `@details` — add the new chart.

> `getPersonCountsIncidenceFilters()` has no dedicated chart in this plan
> (same as `getPersonCountsFilters()` today, which the report doesn't chart
> directly either — its sex/age/visit breakdowns are already covered by the
> dedicated pie/histogram/barplot helpers reading the *non*-incidence
> `getPersonCountsFilters_memoise`). If an incidence-only sex/age/visit
> breakdown is wanted later, it reuses those same plotting helpers against
> `getPersonCountsIncidenceFilters_memoise`'s output — same column names, no
> helper changes needed.

## Part 7 — Docs

- `devtools::document()` — new exported functions: `getPersonCountsIncidence`,
  `getPersonCountsIncidence_memoise`, `getPersonCountsIncidenceFilters`,
  `getPersonCountsIncidenceFilters_memoise`,
  `createIncidencePlotFromPersonCounts` (if Part 6 is included). Watch for the
  roxygen2-version-mismatch reformatting of unrelated `.Rd` files / `DESCRIPTION`
  seen on the Prevalence PR — revert those with `git checkout --` before
  committing.
- `development/STRUCTURE.md`:
  - §3 getters table — two new rows, reusing the exact phrasing style of the
    Prevalence/Filters rows above them.
  - §3 *Output shapes* — new `#### getPersonCountsIncidence() → one tibble`
    and `#### getPersonCountsIncidenceFilters() → one tibble` subsections,
    each stating the "first is absolute, filters apply after" rule once
    (Decision 2) rather than repeating it on every column).
  - §3 *Reports & plots* — the new chart (if Part 6 included).
  - §4 — two new rows in the endpoints table.
- `NEWS.md` — one bullet.

## Verification

1. `devtools::document()` — clean (after reverting unrelated reformatting).
2. `HADESEXTAS_TESTING_ENVIRONMENT=OnlyCounts-FinnGen devtools::test()` — no
   fixture regeneration needed, should run immediately.
3. `HADESEXTAS_TESTING_ENVIRONMENT=AtlasDevelopment-5k devtools::test()` —
   confirms the correlated-subquery SQL translates on BigQuery.
4. `devtools::check()` — new exports touch `NAMESPACE`/`man`.
5. Manual smoke: `runApiServer(buildCountsTable = FALSE)` against
   `OnlyCounts-FinnGen` (or the bundled Eunomia DB built with
   `buildCountsTable = TRUE`), then `GET /getPersonCountsIncidence?conceptIds=317009SD`,
   `GET /getPersonCountsIncidenceFilters?conceptIds=317009SD`, and
   `GET /report?conceptId=317009` (new chart renders, if Part 6 included).

## Notes / caveats

- **"Incident" means "first record in `stratified_persons`"**, which is itself
  bounded by whatever history the CDM holds — a person whose first-ever
  real-world occurrence predates the CDM's `observation_period` coverage will
  show as incident on their first *recorded* year instead. Same caveat
  Prevalence already documents for "recorded" vs "true" prevalence; worth
  repeating in the roxygen for Incidence since it matters even more there (a
  single mis-dated "first" event, unlike prevalence, affects every later
  year's count for that person too — they can never be incident again for
  that token).
- **Rows can legitimately be all-zero for a token** once `yearsRange` excludes
  its one incident year per person — this is correct (Decision 2), not a bug;
  called out explicitly in the roxygen and covered by a dedicated test (Part 5).
- Possible follow-up (out of scope, flagged in Decision 1): a true
  epidemiological incidence-rate denominator that excludes previously-incident
  persons, if the simpler reading proves insufficient later.
