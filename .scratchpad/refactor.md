# Endpoints refactor — execution plan

## Context

Original note (design intent, kept verbatim for reference):

- `getConceptsWithCodeCounts()` → rename `getAllConceptsInfo()`; strip
  `record_counts`, `descendant_record_counts`, `number_of_descendants`,
  `person_counts`, `descendant_person_counts` from its output — it becomes a
  pure concept-metadata catalogue.
- `getConceptRelationships(conceptId)` → what is now `getCodeCounts()$concept_relationships`
  + `getCodeCounts()$concepts`.
- `getCodeCountsStratified(conceptId)` → what is now `getCodeCounts()$stratified_code_counts`.
- `getPersonCountsFilters(conceptId, years_range)` → what is now `getPersonCounts()$filter_person_counts`,
  restricted to the concept + all its descendants, over `years_range` (empty ⇒ full range). Reuse the
  memoised tree getter, don't recompute the tree.
- `getPersonCountsUpset(conceptId, years_range, level, sex, age, visit_type)` → what is now
  `getPersonCounts()$upset_person_counts`.

`getCodeCounts()` and `getPersonCounts()` are each being split in two; both
already delegate tree-building to the shared `getConceptTree_memoise()`
(`R/getConceptTree.R`), so splitting them does **not** duplicate the tree
query — each half just calls the same memoised getter independently.

Current call graph (from reading `R/getCodeCounts.R`, `R/getPersonCounts.R`,
`R/getConceptsWithCodeCounts.R`, `R/getConceptTree.R`, `inst/plumber/plumber.R`,
`R/createReport.R`, `inst/reports/testReport.Rmd`, `R/plotingFunctions.R`,
`R/runApiServer.R`):

- `getCodeCounts(CDMdbHandler, conceptId, codeCountsTable)` returns
  `list(concept_relationships, stratified_code_counts, concepts)`. Internally:
  1. `conceptTree <- getConceptTree_memoise(...)` → `family_tree`, `concept_ids`.
  2. Queries `stratified_@codeCountsTable` for `concept_id, maps_to_concept_id,
     strata cols, record_counts` over the tree's concept ids → `codeCounts`.
  3. Derives `mappings` (Maps to / Mapped from edges) from `codeCounts`'
     distinct `(concept_id, maps_to_concept_id)` pairs.
  4. `familyTreeWithMappings <- family_tree + mappings edges`.
  5. Collapses `codeCounts` across `maps_to_concept_id` → `codeCountsStandard`,
     unions with source-concept rows → `codeCountsPerId`.
  6. Builds `familyTreeDescendants` (family_tree minus mapping/-1/0 edges — note
     mapping edges never appear in the plain `family_tree`, only in
     `familyTreeWithMappings`, so this step doesn't actually need the mappings
     at all) and an ancestor closure per node → `ancestorTableOfDescendant`.
  7. Sums `codeCountsPerId` over each node's descendant set →
     `nodeDescendantRecordCounts`; full-joins with `codeCountsPerId` →
     `stratifiedCodeCounts` (`node_record_counts`, `node_descendant_record_counts`).
  8. Calls `getConceptsWithCodeCounts_memoise(...)` (the **global** catalogue,
     including counts) and `select(-number_of_descendants)`, left-joins onto
     the tree's distinct `child_concept_id` → `concepts`.
     - **Dead code found here**: a `missingConcepts` tibble is computed (fills
       NA concept metadata for concepts absent from `concept`, e.g. on
       Eunomia) but is never assigned back to `concepts` or returned — it's
       inert. Carry it over as-is when splitting (preserve current behavior
       exactly); do not silently fix it as part of this refactor. Flag to the
       user separately.
  9. `conceptRelationships <- familyTreeWithMappings` + `concept_class_id`
     joined in, `paths` dropped.
  - Returns `concept_relationships`, `stratified_code_counts`, `concepts`.

- `getPersonCounts(CDMdbHandler, conceptId, level, sexStratum, ageStratum,
  yearStratum, visitStratum, codeCountsTable)` returns
  `list(filter_person_counts, upset_person_counts)`. Internally:
  1. Same shared `getConceptTree_memoise(...)`.
  2. `overLevelConceptIds` — tree ids pruned to `level` (NULL ⇒ full tree).
  3. `filter_person_counts` — one SQL query over `stratified_persons`
     restricted to the **full tree**'s concept ids (not level-pruned), no
     stratum filters applied — sex/age/visit breakdown, independent of
     `level`/stratum args.
  4. `upset_person_counts` — pulls `(person_id, target_concept_id)` membership
     rows from `stratified_persons`, restricted to `overLevelConceptIds` **and**
     the stratum filters (`sexStratum`, `ageStratum`, `yearStratum`,
     `visitStratum`, via `.inFilterSql()`), groups in R into exclusive regions.
  - `yearStratum` today is an arbitrary **discrete set** of years (IN-list),
    applied only to `upset_person_counts`, not to `filter_person_counts`.

- `getConceptsWithCodeCounts(CDMdbHandler, codeCountsTable)` — one SQL call:
  `concept` INNER JOIN `code_counts` → concept metadata + `record_counts`,
  `descendant_record_counts`, `number_of_descendants`, `person_counts`,
  `descendant_person_counts`. Consumers:
  - `R/runApiServer.R:72` — warms the memoise cache on server start
    (`getConceptsWithCodeCounts_memoise(CDMdbHandler)`), doesn't use the result.
  - `inst/plumber/plumber.R` `/getListOfConcepts` — selects `concept_id,
    concept_name, vocabulary_id, concept_code, number_of_descendants`.
  - `R/getCodeCounts.R` (step 8 above) — the only place that reads the count
    columns from this catalogue.

- `inst/reports/testReport.Rmd` calls `getCodeCounts_memoise(...)` once into a
  `results` list, then threads that same `results` object through
  `pruneLevelsFromResults()`, `createMermaidGraphFromResults()`,
  `createCodeCountsTableFromResults()`, `createPlotFromResults()`, and
  `createUpsetPlotFromPersonCounts(personCounts$upset_person_counts,
  results$concepts)` — all of `R/plotingFunctions.R` is written against the
  3-key `list(concept_relationships, stratified_code_counts, concepts)` shape.
  **Out of scope for this refactor** to rewrite `plotingFunctions.R` itself —
  the report will reassemble that same 3-key shape from the two new getters.

---

## Part A — `getConceptsWithCodeCounts()` → `getAllConceptsInfo()`

- New file `R/getAllConceptsInfo.R`; delete `R/getConceptsWithCodeCounts.R`.
- Rename the function and its memoised twin
  (`getAllConceptsInfo_memoise <- memoise::memoise(getAllConceptsInfo, omit_args
  = "CDMdbHandler")`).
- Drop `cc.record_counts, cc.descendant_record_counts, cc.number_of_descendants,
  cc.person_counts, cc.descendant_person_counts` from the `SELECT`. Keep the
  `INNER JOIN` to `@resultsDatabaseSchema.@codeCountsTable` — it's still what
  restricts the catalogue to concepts that actually have counts; only the
  count *values* are dropped, not the filter. Also drop the now-unused
  `codeCountsTable` doc language around counts (keep the param — it still
  selects the join table).
- Output columns: `concept_id`, `concept_name`, `domain_id`, `vocabulary_id`,
  `concept_class_id`, `standard_concept`, `concept_code`.
- Update roxygen `@return` list accordingly.

**Callers to update:**
- `R/runApiServer.R:72` → `getAllConceptsInfo_memoise(CDMdbHandler)`.
- `inst/plumber/plumber.R` `/getListOfConcepts` (~L102-109) → call
  `getAllConceptsInfo_memoise`; **drop `number_of_descendants` from the
  `dplyr::select()`** — confirmed with the user: no frontend consumes it today,
  so it's dropped outright, no replacement join needed. Response becomes
  `concept_id, concept_name, vocabulary_id, concept_code`.
- `R/getCodeCounts.R` step 8 — moves into Part B below; stops calling this
  getter altogether (see Part B, it queries `code_counts` directly instead).

**Tests:** rename `tests/testthat/test-getConceptsWithCodeCounts.R` →
`test-getAllConceptsInfo.R`; update the function name and drop assertions on
the five removed columns.

**Docs:** `development/STRUCTURE.md` §3 getters table — rename the row, update
its "Reads from" (drop the implication it returns counts) and the `#### Output
shapes` section for `getConceptsWithCodeCounts()` → `getAllConceptsInfo()`,
7 columns instead of 12.

---

## Part B — split `getCodeCounts()` → `getConceptRelationships()` + `getCodeCountsStratified()`

Delete `R/getCodeCounts.R` (and its memoised export) once both replacements
exist and all callers are moved over.

### B1 — `R/getConceptRelationships.R`

```
getConceptRelationships(CDMdbHandler, conceptId, codeCountsTable = "code_counts")
  -> list(concept_relationships, concepts)
getConceptRelationships_memoise <- memoise::memoise(getConceptRelationships, omit_args = "CDMdbHandler")
```

Carries over steps 1, 3, 4, 8, 9 from the current `getCodeCounts()` (see
Context), with one change to step 8: since `getAllConceptsInfo()` no longer
carries counts, this function issues its **own** scoped query against
`code_counts` instead of going through the (now count-less) global catalogue:

```sql
SELECT DISTINCT c.concept_id, c.concept_name, c.domain_id, c.vocabulary_id,
       c.concept_class_id, c.standard_concept, c.concept_code,
       cc.record_counts, cc.descendant_record_counts, cc.number_of_descendants,
       cc.person_counts, cc.descendant_person_counts
FROM @vocabularyDatabaseSchema.concept c
INNER JOIN @resultsDatabaseSchema.@codeCountsTable cc ON c.concept_id = cc.concept_id
WHERE c.concept_id IN (@conceptIds)
```
(`@conceptIds` = the tree's distinct `child_concept_id`s.) This is strictly
cheaper than before — the old code pulled the memoised **global** catalogue
and then joined/filtered in R; this scopes the query itself. Preserves the
same `standard_concept` NA→FALSE mutate and the dead `missingConcepts` block
verbatim (see Context caveat — not fixing that here).

For step 3 (`mappings`), this function needs `codeCounts`' distinct
`(concept_id, maps_to_concept_id)` pairs — query only those two columns
(`SELECT DISTINCT concept_id, maps_to_concept_id FROM stratified_@codeCountsTable
WHERE concept_id IN (@conceptIds) OR maps_to_concept_id IN (@conceptIds)`), not
the full strata+`record_counts` row set (that belongs to Part B2 only).

Returns `list(concept_relationships = conceptRelationships, concepts = concepts)`.

### B2 — `R/getCodeCountsStratified.R`

```
getCodeCountsStratified(CDMdbHandler, conceptId, codeCountsTable = "code_counts")
  -> tibble (the current stratified_code_counts shape: node_record_counts, node_descendant_record_counts, per stratum)
getCodeCountsStratified_memoise <- memoise::memoise(getCodeCountsStratified, omit_args = "CDMdbHandler")
```

Carries over steps 1, 2, 5, 6, 7 from Context. Confirms the observation above:
`familyTreeDescendants` only needs the **plain** `family_tree` (from
`getConceptTree_memoise`) filtered to drop `"-1"`/`"0"` levels — mapping edges
are never present in the plain tree, so this function needs **no** mappings
derivation at all, and doesn't need the full `getConceptRelationships()` output
— it only shares the memoised tree getter.

Returns the tibble directly (not wrapped in a list — it was always a single
tibble under `getCodeCounts()$stratified_code_counts`).

### B3 — Callers

- `inst/plumber/plumber.R`: remove the `/getCodeCounts` endpoint (~L24-45);
  add two endpoints:
  ```
  #* @get /getConceptRelationships
  function(res, conceptId=0L) { ... getConceptRelationships_memoise(...) ... }

  #* @get /getCodeCountsStratified
  function(res, conceptId=0L) { ... getCodeCountsStratified_memoise(...) ... }
  ```
  Same `conceptId` validation/`tryCatch`/400 pattern as the current
  `/getCodeCounts` block.
- `inst/reports/testReport.Rmd` (~L31-40): replace
  ```r
  results <- getCodeCounts_memoise(CDMdbHandler, conceptId = conceptId)
  ```
  with
  ```r
  relationships <- getConceptRelationships_memoise(CDMdbHandler, conceptId = conceptId)
  results <- list(
    concept_relationships = relationships$concept_relationships,
    concepts = relationships$concepts,
    stratified_code_counts = getCodeCountsStratified_memoise(CDMdbHandler, conceptId = conceptId)
  )
  ```
  so `results` keeps the exact 3-key shape `pruneLevelsFromResults()` /
  `createMermaidGraphFromResults()` / `createCodeCountsTableFromResults()` /
  `createPlotFromResults()` / `results$concepts` (used by
  `createUpsetPlotFromPersonCounts`) already expect — **no changes needed in
  `R/plotingFunctions.R`**.
- `R/createReport.R` — no signature change; update the `@details` bullet that
  says "Retrieving code counts and concept relationships using
  `getCodeCounts_memoise`" to name the two new getters instead.

**Tests:** split `tests/testthat/test-getCodeCounts.R` into
`test-getConceptRelationships.R` (assertions on `concept_relationships` +
`concepts` shape/columns) and `test-getCodeCountsStratified.R` (assertions on
the stratified tibble, incl. the `node_hll_person_counts`-era column list
already present there). Keep the same `skip_if(testingDatabase != ...)` guards
(this is a post-counts-tables function family → OnlyCounts-FinnGen +
AtlasDevelopment-5k only, per CLAUDE.md's testing-database table).

**Docs:** `development/STRUCTURE.md` §3 — replace the `getCodeCounts()` row and
its `#### getCodeCounts() → list of three tibbles` section with two entries:
`getConceptRelationships()` (`concept_relationships` + `concepts`) and
`getCodeCountsStratified()` (the stratified tibble). Update the endpoint table
in §4 (drop `/getCodeCounts`, add `/getConceptRelationships`,
`/getCodeCountsStratified`).

---

## Part C — split `getPersonCounts()` → `getPersonCountsFilters()` + `getPersonCountsUpset()`

Delete `R/getPersonCounts.R` (and its memoised export) once both replacements
exist and all callers are moved over. Keep the `.inFilterSql()` helper — both
new functions need it — either duplicated into each file or moved to a shared
small internal-helpers file (prefer: keep in whichever file needs it first,
duplicate the ~6-line static helper into the other; not worth a new shared
file for one static helper — matches the "no premature abstraction" rule).

**Design decision required by the note, applied here:** `years_range` is a
**range** (`c(startYear, endYear)`), not a discrete set — a deliberate change
from today's `yearStratum`, which is an arbitrary discrete IN-list applied
*only* to `upset_person_counts`. Under the new functions:
- `getPersonCountsFilters()` gains a year filter it didn't have before (today's
  `filter_person_counts` ignores year entirely).
- `getPersonCountsUpset()`'s `yearStratum` (discrete set) becomes `yearsRange`
  (inclusive range) — a behavior change, not just a rename. Flag this
  explicitly when it lands.

Validate `yearsRange` as `NULL` or an integer vector of length 2
(`checkmate::assertIntegerish(len = 2, null.ok = TRUE)`); empty/NULL ⇒ no year
filter (full range), per the note ("if year_range empty use all the range").
**Confirmed wire format:** comma pair (`yearsRange=2015,2020`), matching the
existing `parseIntCsv` convention used by every other `*Stratum` query param.

**Confirmed validation, applied identically in both C1 and C2** (R-function
level, via `checkmate` + an explicit `stop()` for the ordering check that
`checkmate` can't express in one call, same pattern as the rest of the file):
```r
yearsRange |> checkmate::assertIntegerish(len = 2, null.ok = TRUE)
if (!is.null(yearsRange) && yearsRange[1] > yearsRange[2]) {
    stop("yearsRange: first year must be <= second year")
}
```
The plumber handler's existing `tryCatch(..., error = function(e) { res$status
<- 400; ... })` pattern (already used by every endpoint) surfaces this as a
400 with the message — no separate range check needed in the plumber layer
itself, only the comma-split-into-length-2 parse (see C3).

### C1 — `R/getPersonCountsFilters.R`

```
getPersonCountsFilters(CDMdbHandler, conceptId, yearsRange = NULL, codeCountsTable = "code_counts")
  -> tibble (filter, stratum, person_counts) -- same shape as today's filter_person_counts
getPersonCountsFilters_memoise <- memoise::memoise(getPersonCountsFilters, omit_args = "CDMdbHandler")
```

Steps 1 and 3 from Context, with the `tree_persons` CTE gaining a year filter:
```sql
WITH tree_persons AS (
    SELECT DISTINCT person_id, gender_concept_id, age_decile, visit_group_concept_id
    FROM @resultsDatabaseSchema.@stratifiedPersonsTable
    WHERE (concept_id IN (@conceptIds) OR maps_to_concept_id IN (@conceptIds))
      @yearFilterSql
)
```
where `@yearFilterSql` is `""` when `yearsRange` is NULL, else
`AND calendar_year BETWEEN @minYear AND @maxYear` (reuse/adapt the existing
`.inFilterSql`-style builder — a `.betweenFilterSql()` variant since this one
needs a range, not an IN-list). Still uses the **full tree**'s concept ids
(`treeConceptIds` from `getConceptTree_memoise`, not level-pruned) — the note
says "the concept with all the descendants", matching today's behavior of not
level-pruning `filter_person_counts`.

### C2 — `R/getPersonCountsUpset.R`

```
getPersonCountsUpset(CDMdbHandler, conceptId, yearsRange = NULL, level = NULL,
                      sexStratum = NULL, ageStratum = NULL, visitStratum = NULL,
                      codeCountsTable = "code_counts")
  -> tibble (group, person_counts) -- same shape as today's upset_person_counts
getPersonCountsUpset_memoise <- memoise::memoise(getPersonCountsUpset, omit_args = "CDMdbHandler")
```

Steps 1, 2, 4 from Context. `strataFilterSql` swaps its `yearStratum` IN-list
term for the same `@yearFilterSql` BETWEEN-range builder as C1 (shared logic —
factor `.betweenFilterSql(column, range)` next to `.inFilterSql()`, whichever
file ends up "primary"; duplicate the few lines into the other rather than add
a new shared file, per house style).

### C3 — Callers

- `inst/plumber/plumber.R`: remove `/getPersonCounts` (~L48-92); add:
  ```
  #* @get /getPersonCountsFilters
  #* @param conceptId ...
  #* @param yearsRange Comma-separated "startYear,endYear". Omit for the full range
  function(res, conceptId = 0L, yearsRange = "") { ... }

  #* @get /getPersonCountsUpset
  #* @param conceptId ...
  #* @param yearsRange Comma-separated "startYear,endYear". Omit for the full range
  #* @param level Maximum tree depth. Omit for the full tree
  #* @param sexStratum / ageStratum / visitStratum Comma-separated ids to restrict to
  function(res, conceptId = 0L, yearsRange = "", level = "",
           sexStratum = "", ageStratum = "", visitStratum = "") { ... }
  ```
  Reuse the existing `parseIntCsv()` closure (currently inline in the
  `/getPersonCounts` handler) for the stratum params; add a small
  `parseYearsRange()` that splits `"2015,2020"` on `,`, and 400s if the result
  isn't exactly 2 values (empty string ⇒ `NULL`, same as `parseIntCsv`) — this
  is just the parse/shape check; the first-year-<=-second-year ordering check
  lives in the R functions themselves (C1/C2 above) and surfaces as a 400 via
  the existing `tryCatch`.
- `inst/reports/testReport.Rmd` (~L61-83): replace
  ```r
  personCounts <- getPersonCounts_memoise(CDMdbHandler, conceptId = conceptId)
  ```
  with
  ```r
  personCounts <- list(
    filter_person_counts = getPersonCountsFilters_memoise(CDMdbHandler, conceptId = conceptId),
    upset_person_counts = getPersonCountsUpset_memoise(CDMdbHandler, conceptId = conceptId)
  )
  ```
  (no `yearsRange`/`level`/stratum args passed from the report today, so
  defaults apply — behavior unchanged for the report). No changes needed in
  `R/plotingFunctions.R`'s `createSexPieChartFromPersonCounts()` /
  `createAgeHistogramFromPersonCounts()` / `createVisitBarplotFromPersonCounts()`
  / `createUpsetPlotFromPersonCounts()` — they only ever consumed
  `personCounts$filter_person_counts` / `personCounts$upset_person_counts`
  individually, never the wrapping list structure itself.

**Tests:** split `tests/testthat/test-getPersonCounts.R` into
`test-getPersonCountsFilters.R` and `test-getPersonCountsUpset.R`; add cases
for `yearsRange` (NULL ⇒ unchanged totals vs. today's no-year-filter behavior;
a real range ⇒ narrower totals). Same DB-stage guards as Part B (post-counts
functions only).

**Docs:** `development/STRUCTURE.md` §3 — replace the `getPersonCounts()` row
and its `#### getPersonCounts() → list of two tibbles` section with two
entries; note the `yearsRange` param and its range (not set) semantics in both.
Update §4 endpoint table: drop `/getPersonCounts`, add
`/getPersonCountsFilters`, `/getPersonCountsUpset`.

---

## Part D — cross-cutting cleanup

- `devtools::document()` after all four new files exist and the three old
  ones are deleted — regenerates `NAMESPACE` (removes
  `getConceptsWithCodeCounts`/`_memoise`, `getCodeCounts`/`_memoise`,
  `getPersonCounts`/`_memoise` exports; adds the six new function +
  `_memoise` export pairs) and `man/*.Rd`. Per the established project
  workaround (roxygen2 8.1.0 installed vs. `RoxygenNote: 7.3.3` pinned causes
  large reformatting diffs), hand-edit `man/` instead if `document()` produces
  unrelated churn — mirror whichever approach was used for the HLL/person-bridge
  migration's man pages.
- `grep -rn "getConceptsWithCodeCounts\|getCodeCounts\b\|getPersonCounts\b" R/
  inst/ tests/ development/` after the edits — every remaining hit should be a
  deliberate reference to one of the six new names, not a stale one.
- `CLAUDE.md` has no direct references to these function names (confirmed) —
  no edit needed there.

---

## Verification

1. `devtools::load_all(".")` — no errors, no leftover reference to the three
   removed functions.
2. `devtools::test()` — new/renamed test files pass on the DB stages CLAUDE.md
   assigns to post-counts functions (OnlyCounts-FinnGen, AtlasDevelopment-5k).
3. `ROMOPAPI::runApiServer()` (bundled Eunomia), then:
   - `GET /getListOfConcepts` — no `number_of_descendants` column any more;
     confirm this is the intended, called-out shape change.
   - `GET /getConceptRelationships?conceptId=317009` and
     `GET /getCodeCountsStratified?conceptId=317009` — together reconstruct
     what `/getCodeCounts?conceptId=317009` used to return.
   - `GET /getPersonCountsFilters?conceptId=317009&yearsRange=2015,2020` and
     `GET /getPersonCountsUpset?conceptId=317009&yearsRange=2015,2020&level=1`
     — narrower totals than the unfiltered call.
   - `GET /report?conceptId=317009` — still renders (mermaid tree, tables,
     plots, person-counts section) unchanged, since `plotingFunctions.R` and
     the Rmd's downstream chunks are untouched.
4. `devtools::check()` clean (same baseline as before: no new WARNINGs/NOTEs).

## Resolved decisions (confirmed with the user)

- `/getListOfConcepts` drops `number_of_descendants` outright — no frontend
  consumes it, no replacement source needed.
- `yearsRange` is wire-encoded as a comma pair (`yearsRange=2015,2020`), and
  both `getPersonCountsFilters()`/`getPersonCountsUpset()` validate
  `yearsRange[1] <= yearsRange[2]` (400 via the existing `tryCatch` pattern if
  violated) — see Part C.

## Still open

- The dead `missingConcepts` block in current `getCodeCounts()` (computed,
  never used — see Context/Part B1): it fills NA concept metadata
  (`concept_name`, `domain_id`, etc.) with placeholders for concepts present in
  the family tree but missing from `concept`/`code_counts` (happens on
  Eunomia's small test vocabulary), but the result is never assigned back to
  `concepts` or returned, so today's actual output still has raw NAs. Recommend
  wiring it in (`concepts <- missingConcepts` at the end of
  `getConceptRelationships()`) while touching this code anyway, since it looks
  like an unfinished fix rather than intentionally-inert code — but this is a
  behavior change beyond the requested rename/split, so confirm before Part B1
  lands whether to (a) wire it in, (b) delete it as unused, or (c) carry it
  over inert as originally planned.
