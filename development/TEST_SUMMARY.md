# Test suite summary

One section per file in `tests/testthat/`, one subsection per `test_that()`. Each
subsection title is tagged with the test's **category** — see legend below.
Tags are what the test actually checks, not what it's testing the presence of;
several tests earn more than one tag.

## Category legend

| Tag | Means |
|-----|-------|
| **synthetic** | Runs against a small hand-built fixture (a tribble, or a throwaway SQLite table) whose correct answer is known by construction — the assertion is an *exact* expected value, not a bound. |
| **behavioral** | Checks output shape: column names, non-emptiness, types, no unexpected `NA`s. Doesn't verify the underlying arithmetic. |
| **invariant** | Checks a mathematical relationship that must hold regardless of the actual data (A ⊆ B, sum ≤ total, cross-consistency between two functions/tables) — not a hand-computed number. |
| **validation** | Feeds malformed/invalid input and asserts the function rejects it (`expect_error`). |
| **integration** | Exercises the real build-or-read pipeline end-to-end against a live CDM (creation from Eunomia/BigQuery, or full report rendering) — a change anywhere in that path can break it. |
| **fixture** | Reads the shipped precomputed `OnlyCounts-FinnGen` (or built `AtlasDevelopment-5k`) data and checks properties of the real numbers it returns, without an independently computed ground truth. |
| **regression** | Targets one specific, previously-identified edge case/bug rather than general behavior. |
| **memoisation** | Checks that a `_memoise` twin caches/returns consistently. |

## Shared test infrastructure (not test files)

- **`setup.R`** — picks the active `testingDatabase` (`Eunomia-GiBleed`,
  `OnlyCounts-FinnGen`, `AtlasDevelopment-5k`, `AtlasDevelopment-full`) from
  `HADESEXTAS_TESTING_ENVIRONMENT` and builds `test_cohortTableHandlerConfig`;
  defines `creationDatabases`/`postCountsDatabases`, which every DB-dependent
  test below gates on via `skip_if(_not)`.
- **`helper.R`** — `.buildSyntheticPersonCountsHandler()`, a hand-built,
  throwaway-SQLite `CDMdbHandler` (6 persons, 2 concept trees; persons 1 and 6
  each carry events under *both* trees, so exclusive-region math has a real
  overlap to get right, not only disjoint singletons) used by the
  **synthetic** tests in `test-getPersonCountsUpset.R` and
  `test-getPersonCountsFilters.R`.

---

## `test-createCodeCountsTable.R`

### "createStratifiedCodeCountsTable works with duplicated counts" — *(integration, behavioral)*
Builds `stratified_code_counts` from a real CDM (Eunomia/BigQuery) and checks
the table is non-empty, has the expected columns, and carries no rows with
visit grouping applied (grouping disabled here).

### "createStratifiedCodeCountsTable works with visit_source_group_concept_ids" — *(integration, behavioral)*
BigQuery-only. Same build, this time with the FinnGen visit-group list passed
in; checks no row is left ungrouped and every group id comes from the
configured list.

### "served stratified_code_counts is grouped by visit_source_group_concept_ids" — *(fixture, invariant)*
No build step — reads the already-shipped/pre-built table and checks grouping
was actually applied: no ungrouped rows, more than one group present, and the
groups seen are a subset of the configured FinnGen list.

### "createStratifiedCodeCountsTable works with visit_source_group_concept_ids if one missing takes childern" — *(integration, behavioral)*
Drops one visit group from the configured list and checks its child visit
types get grouped into something else instead of vanishing.

### "createCodeCountsTables works" — *(integration, invariant)*
Runs the full build pipeline on a real CDM, then checks `code_counts`'
cross-column invariants: `descendant_record_counts >= record_counts`,
`number_of_descendants >= 1`, the record/descendant-count-equality case
implies exactly 1 descendant (and vice versa), `descendant_person_counts >=
person_counts`, and the person-count analog of the record-count equality
case.

### "createCodeCountsTables works stratified by visit_group_concept_id" — *(integration, invariant)*
Same as above, with visit-group stratification enabled (BigQuery-only).

### "stratified table keeps source concepts with no standard concept (concept_id = 0)" — *(synthetic, regression)*
Hand-built throwaway SQLite CDM with exactly one mapped and one
unmapped-source (`concept_id = 0`) condition row. Asserts, by exact concept
id, that the unmapped event survives into `stratified_code_counts` and
`code_counts` under its source concept, and that no events get silently
merged into a phantom `concept_id = 0` row.

---

## `test-createReport.R`

### "createReport works with basic parameters" — *(integration)*
Renders the full `/report` Rmd end-to-end from the real fixture and checks it
produced a non-empty `.html` file.

### "createReport works with all parameters" — *(integration)*
Same, exercising `showsMappings`, `pruneLevels`, and `pruneClass` together.

---

## `test-createStratifiedPersonsTable.R`

### "createStratifiedPersonsTable works" — *(integration, invariant)*
Builds the person-bridge table from a real CDM; checks columns, non-emptiness,
no visit-grouped rows (grouping disabled), and — the key invariant for this
table — no duplicate rows at the `person_id × concept_id × stratum` grain.

### "createStratifiedPersonsTable works with visit_source_group_concept_ids" — *(integration, invariant)*
Same, with visit grouping enabled; checks no ungrouped rows remain and every
group id is drawn from the configured FinnGen list.

---

## `test-getAPIInfo.R`

### "getAPIInfo works" — *(behavioral, fixture)*
Checks the 4 expected fields are returned and that `romop_api_version`
matches the actually-installed package version.

---

## `test-getAllConceptsInfo.R`

### "getAllConceptsInfo works" — *(behavioral, fixture)*
Checks non-emptiness, the expected column set, no `NA` in required columns,
correct types (`standard_concept` logical, `concept_id` double), and that
`concept_id` is unique across rows.

---

## `test-getCodeCountsStratified.R`

### "getCodeCountsStratified works" — *(behavioral, invariant, fixture)*
Checks columns/non-`NA`, that every returned `concept_id` also appears in
`getConceptRelationships()`'s `concepts` tibble (cross-function consistency),
and that summing the per-stratum counts reproduces a row per concept (an
internal aggregation-consistency check, not a hand-computed number).

### "getCodeCountsStratified returns error if conceptId is not found" — *(validation)*

---

## `test-getConceptRelationships.R`

### "getConceptRelationships works" — *(behavioral, invariant, fixture)*
Checks columns/non-`NA` on both returned tibbles, and two structural tree
invariants: every `parent_concept_id` is itself some edge's child (no
dangling parent), and every `child_concept_id` has a matching row in
`concepts`.

### "getConceptRelationships returns error if conceptId is not found" — *(validation)*

---

## `test-getConceptTree.R`

### ".familyTreeToAncestorTable works" — *(synthetic)*
Hand-built 14-row family tree tribble with a fully known expected ancestor
table computed for three different root concepts (including leaf and
mid-tree roots) — pure R function, no DB involved, exact expected output.

### "getConceptTree works" — *(behavioral, fixture, memoisation)*
Checks shape/columns, that the root concept is present and the reverse-parent
`"-1"` edge is excluded from `concept_ids`, and that `getConceptTree_memoise`
returns an identical result to the unmemoised call.

---

## `test-getPersonCountsFilters.R`

### "getPersonCountsFilters rejects malformed conceptIds tokens" — *(validation)*
Empty string, non-matching text, and a token with no `S`/`M` tag all error.

### "getPersonCountsFilters works" — *(behavioral, fixture)*
Checks the 4-column shape (incl. the new `selected`), that `filter` only
takes the 4 known dimension values, no `NA`s, and that nothing is `selected`
when no stratum filters are passed.

### "getPersonCountsFilters gives exact pooled breakdown counts on a hand-built fixture (synthetic)" — *(synthetic)*
Against `.buildSyntheticPersonCountsHandler()` (`helper.R`): the pooled
6-person population's unfiltered sex/age/visit/year breakdown matches the
hand-computed count for every stratum in all 4 dimensions exactly.

### "getPersonCountsFilters reciprocal filtering matches hand-computed subsets on a hand-built fixture (synthetic)" — *(synthetic)*
Applying `sexStratum = 1` on the synthetic fixture produces the exact
hand-computed age breakdown for the male subset (1 person per age decile),
while the `sex` dimension's own total still sums to all 6 persons.

### "getPersonCountsFilters selected flags mark exactly the passed-in filter values on a hand-built fixture (synthetic)" — *(synthetic)*
Passing `sexStratum` and `yearsRange` together on the synthetic fixture
produces the exact expected `TRUE`/`FALSE` pattern across the `sex` and
`year` rows, and confirms `age`/`visit` (no filter passed) have nothing
selected.

### "getPersonCountsFilters marks the selected stratum and cross-filters the other dimensions" — *(invariant, fixture)*
Passes a `sexStratum` and checks: `selected` is `TRUE` exactly on the matching
sex row; the `age` breakdown total shrinks under the sex filter; the `sex`
dimension's own total is unaffected by its own filter (reciprocal-filtering
invariant, on real data — not a hand-computed number).

### "getPersonCountsFilters marks selected year strata from yearsRange" — *(invariant, fixture)*
Same reciprocal-filtering check for the `year` dimension via `yearsRange`.

### "getPersonCountsFilters rejects an inverted yearsRange" — *(validation)*

### "getPersonCountsFilters returns error if concept id has no descendants" — *(validation)*
A `...D` token on a nonexistent concept id (empty descendant expansion).

---

## `test-getPersonCountsUpset.R`

### "getPersonCountsUpset rejects malformed conceptIds tokens" — *(validation)*
Empty string, non-matching text, and a bare id with no tag all error.

### "getPersonCountsUpset works for a single descendant-expanded set" — *(behavioral, invariant, fixture)*
Checks the 2-column shape, no `NA`s, a single set produces a single
(self-labeled) region, and that region's count is bounded above by
`code_counts`' `descendant_person_counts` for the same concept.

### "getPersonCountsUpset without D restricts to the exact code" — *(invariant, fixture)*
An `S`-only token's region count is bounded above by `code_counts`'
`person_counts` (the concept's own, non-descendant count).

### "getPersonCountsUpset keeps tokens with the same concept id but different tags distinct" — *(invariant, fixture)*
Structural check that `S ⊆ SD` holds on real data: every returned region
label contains the `SD` token, and no bare `S`-only region exists — without
asserting the actual counts (that's what the synthetic-fixture equivalent
test below does).

### "getPersonCountsUpset gives exact overlapping-region counts on a hand-built fixture (synthetic)" — *(synthetic)*
Against `.buildSyntheticPersonCountsHandler()` (`helper.R`): two persons carry
events under both concept trees, so `"100SD,200MD"` produces a genuine 3-region
split — an overlap region plus two set-only regions — each with the exact
hand-computed count (2/2/2 persons), not just disjoint singletons.

### "getPersonCountsUpset gives exact self-inclusive overlap counts on a hand-built fixture (synthetic)" — *(synthetic)*
On the same fixture, `"100S,100SD"` (`SD` self-inclusive, so `S` is always a
subset of `SD`) produces the exact expected 2-person shared-root region and
2-person descendants-only region.

### "getPersonCountsUpset stratum filters narrow totals consistently with getPersonCountsFilters" — *(invariant, fixture)*
Cross-checks `getPersonCountsUpset`'s totals against
`getPersonCountsFilters`' sex breakdown on the same real concept — an
independent-oracle-style consistency check between two functions reading the
same table, on real (not hand-built) data.

### "getPersonCountsUpset rejects an inverted yearsRange" — *(validation)*

### "getPersonCountsUpset returns error if concept id has no descendants" — *(validation)*

---

## `test-getVisitTypeNames.R`

### "getVisitTypeNames works" — *(behavioral, fixture)*
Checks the 3-column shape, no `NA`s, no `visit_group_concept_id == 0` rows,
and uniqueness of `visit_group_concept_id`.

### "getVisitTypeNames_memoise works" — *(memoisation)*
Two calls through the memoised wrapper return identical results.
