# Redesign `getPersonCountsUpset` / `getPersonCountsFilters`: multi-set tagged concept IDs

## Context

Today both endpoints take a single `conceptId` + `level`, build one pruned
"counted" family tree via `getConceptTree_memoise()`, and compute counts over
*that whole tree* (concept + all descendants, standard + mapped codes lumped
together).

The new design drops the single-tree model. The caller instead passes an
explicit **list of tagged concept references** — each one independently
choosing (a) which column to match (`concept_id` vs `maps_to_concept_id`) and
(b) whether to expand to all descendants first (via `concept_ancestor`) or
match the code directly. `getConceptTree()` / `getConceptTree_memoise()` are
no longer used by either function (they stay in place — `getConceptRelationships()`
and `getCodeCountsStratified()` still need them).

**Tag grammar** — each list item is `<conceptId><S|M><D?>`:

| Tag | Column matched | Descendants? |
|-----|-----------------|--------------|
| `S`  | `concept_id` | no — exact code only |
| `SD` | `concept_id` | yes — code + all descendants (via `concept_ancestor`) |
| `M`  | `maps_to_concept_id` | no — exact mapped-to code only |
| `MD` | `maps_to_concept_id` | yes — mapped-to code + all its descendants |

Example: `conceptIds=317009S,4191479SD,2000403993M,2000403993MD` — four
independent sets (note `2000403993` appears twice, once with `M` and once
with `MD`: these are two *different* sets and must stay distinct).

Both endpoints take the same `conceptIds` list; `getPersonCountsUpset` reports
one exclusive-region breakdown *per set*, while `getPersonCountsFilters` pools
all sets into one combined population and breaks *that* down by stratum.

---

## Open questions to confirm before implementation

1. **Upset group labels: keep the tag or strip it?** The request says the
   response should be "without the tags," but the example input has the same
   raw id (`2000403993`) twice under different tags (`M` and `MD`) — stripping
   tags would make those two sets collide under one label. **Recommendation:**
   label each set with its full original token (e.g. `2000403993M`,
   `2000403993MD`) so sets stay unique and traceable; "without the tags" is
   satisfied in the sense that no *new* decoration is added, just the input
   token echoed back. Confirm this reading, or specify a different labeling
   rule (e.g. a caller-supplied alias per item instead of tags).

Keep the tags in the upsetplot also 

2. **Does `D` include the root concept itself, or only strict descendants?**
   `concept_ancestor` rows always include the ancestor-equals-descendant
   self-row (min_levels_of_separation = 0). **Recommendation:** include self,
   consistent with how `getConceptTree()` already treats "descendants" (see
   `R/getConceptTree.R:57`, `ca.min_levels_of_separation != 0 OR
   ca.ancestor_concept_id = ca.descendant_concept_id`). Confirm.

include self

3. **`getPersonCountsFilters`: does `yearsRange` still apply globally?** The
   new spec adds `sex`/`age`/`visit` cross-filtering but doesn't mention year.
   **Recommendation:** keep `yearsRange` as a hard filter applied to the whole
   pooled population before any stratum breakdown (as today), not as a fourth
   reciprocal dimension with its own `selected` column. Confirm.

as a fourth
   reciprocal dimension with its own `selected` column

4. **Dropping `codeCountsTable` param.** Both functions currently take a
   `codeCountsTable` argument that's only used to build the counted tree via
   `getConceptTree_memoise()`. The new design reads `stratified_persons`
   directly and never looks at `code_counts`, so this parameter becomes dead.
   **Recommendation:** remove it from both signatures (breaking change, but
   the endpoints are already being redesigned). Confirm it's fine to drop
   rather than keep-but-ignore.

drop

5. **Behavior change to flag, not a question:** the *old* default (`conceptId`
   + no `level`) implicitly matched the full descendant tree across **both**
   `concept_id` and `maps_to_concept_id` as one merged set. There is no single
   new tag that reproduces that — a caller wanting the old behavior must pass
   two tokens (`<id>SD,<id>MD`) and, per open question 1, gets them back as two
   separate sets, not one merged region. Worth a heads-up in the PR/changelog;
   no action needed in this plan unless you want a "merge these tokens into one
   set" escape hatch (not requested — omitted here).

   

---

## Part A — Shared tagged-conceptId parsing + descendant expansion

New file `R/parsePersonCountsConceptIds.R` (internal helpers, not exported):

```r
.parsePersonCountsConceptIds <- function(conceptIdsString) {
  # conceptIdsString: e.g. "317009S,4191479SD,2000403993M,2000403993MD"
  # -> tibble(token, concept_id, column, expand_descendants)
  # stop() with a clear message on: empty string, malformed token
  # (must fully match ^\\d+[SM]D?$), duplicate-*exact*-token repeats optional.
}

.expandDescendantConceptIds <- function(CDMdbHandler, conceptIds) {
  # One batched concept_ancestor query for all conceptIds that need expansion
  # (never one query per token):
  #   SELECT ancestor_concept_id, descendant_concept_id
  #   FROM @vocabularyDatabaseSchema.concept_ancestor
  #   WHERE ancestor_concept_id IN (@conceptIds)
  # -> tibble(ancestor_concept_id, descendant_concept_id); caller groups by
  #    ancestor to get each id's resolved descendant set (includes self, see
  #    open question 2).
}

.resolveTaggedConceptIdSets <- function(CDMdbHandler, parsedTokens) {
  # For tokens with expand_descendants == TRUE, resolve via
  # .expandDescendantConceptIds() (one shared batched call for all of them).
  # For expand_descendants == FALSE, the resolved set is just [concept_id].
  # -> the parsed tibble with a new list-column `resolved_ids`.
}
```

Both `getPersonCountsUpset()` and `getPersonCountsFilters()` call these three
helpers; no per-function duplication of the parsing/expansion logic.

---

## Part B — `getPersonCountsUpset()` redesign

`R/getPersonCountsUpset.R`, new signature:

```r
getPersonCountsUpset(
  CDMdbHandler,
  conceptIds,              # character(1), tagged CSV — see grammar above
  yearsRange = NULL,
  sexStratum = NULL,
  ageStratum = NULL,
  visitStratum = NULL
)
```

Removed: `conceptId` (singular), `level`, `codeCountsTable` (see open
questions 1 and 4). `yearsRange`/`sexStratum`/`ageStratum`/`visitStratum`
validation stays exactly as today.

Steps:
1. `checkmate::assertString(conceptIds)`; parse + resolve via Part A helpers
   (`stop()` inside these surfaces as a normal R error, caught by the plumber
   endpoint's existing `tryCatch` → 400, same pattern as today).
2. Build one `SELECT DISTINCT person_id, '<token>' AS target_set` subquery per
   **token** (not per unique concept id — `2000403993M` and `2000403993MD`
   must remain separate subqueries/sets), each filtered by its own resolved id
   list against its own column, plus the shared strata filters
   (`.inFilterSql` / `.betweenFilterSql`, unchanged), `UNION ALL`'d together:
   ```sql
   SELECT DISTINCT person_id, '317009S' AS target_set
   FROM @resultsDatabaseSchema.stratified_persons
   WHERE concept_id IN (317009) <strata filters>
   UNION ALL
   SELECT DISTINCT person_id, '4191479SD' AS target_set
   FROM @resultsDatabaseSchema.stratified_persons
   WHERE concept_id IN (<4191479 + descendants>) <strata filters>
   UNION ALL
   ...
   ```
3. Same R-side grouping as today: pull membership, `group_by(person_id)`,
   `group = paste(sort(unique(target_set)), collapse = "-")`,
   `count(group, name = "person_counts")`.
4. Keep `getPersonCountsUpset_memoise` as-is (just re-memoises the new
   signature).

---

## Part C — `getPersonCountsFilters()` redesign

`R/getPersonCountsFilters.R`, new signature:

```r
getPersonCountsFilters(
  CDMdbHandler,
  conceptIds,              # tagged CSV, same grammar/parser as Part B
  yearsRange = NULL,
  sexStratum = NULL,
  ageStratum = NULL,
  visitStratum = NULL
)
```

Removed: `conceptId` (singular), `codeCountsTable`. Added: `sexStratum`,
`ageStratum`, `visitStratum` (same `checkmate::assertIntegerish(...,
null.ok = TRUE)` pattern as `getPersonCountsUpset`).

Unlike Upset, all tokens are **pooled into one population** (this endpoint
answers "how do these persons break down by stratum," not "how do these sets
overlap"):

1. Parse + resolve tokens (Part A). Build a single `tree_persons` CTE whose
   `WHERE` ORs together every token's `<column> IN (<resolved ids>)`
   predicate, plus the existing `yearsRange` filter (open question 3):
   ```sql
   WITH tree_persons AS (
     SELECT DISTINCT person_id, gender_concept_id, age_decile, visit_group_concept_id
     FROM @resultsDatabaseSchema.stratified_persons
     WHERE (concept_id IN (...) OR maps_to_concept_id IN (...) OR ...) 
       AND <yearsRange filter>
   )
   ```
2. Three reciprocal-filtered aggregations, each excluding *its own* dimension's
   filter but applying the other two:
   ```sql
   SELECT 'sex' AS filter, CAST(gender_concept_id AS BIGINT) AS stratum,
          COUNT(DISTINCT person_id) AS person_counts
   FROM tree_persons WHERE 1=1 <ageStratum filter> <visitStratum filter>
   GROUP BY gender_concept_id
   UNION ALL
   SELECT 'age', CAST(age_decile AS BIGINT), COUNT(DISTINCT person_id)
   FROM tree_persons WHERE 1=1 <sexStratum filter> <visitStratum filter>
   GROUP BY age_decile
   UNION ALL
   SELECT 'visit', CAST(visit_group_concept_id AS BIGINT), COUNT(DISTINCT person_id)
   FROM tree_persons WHERE 1=1 <sexStratum filter> <ageStratum filter>
   GROUP BY visit_group_concept_id;
   ```
3. Add `selected` in R after the query returns:
   ```r
   result |> dplyr::mutate(selected = dplyr::case_when(
     filter == "sex"   & stratum %in% sexStratum   ~ TRUE,
     filter == "age"   & stratum %in% ageStratum   ~ TRUE,
     filter == "visit" & stratum %in% visitStratum ~ TRUE,
     TRUE ~ FALSE
   ))
   ```
   (`stratum %in% NULL` is always `FALSE`, so an omitted filter naturally
   leaves every row in that dimension `selected = FALSE` — no special-casing
   needed.)
4. Output columns: `filter`, `stratum`, `person_counts`, `selected` (one new
   column vs. today).
5. Keep `getPersonCountsFilters_memoise` as-is.

---

## Part D — Plumber endpoint changes

`inst/plumber/plumber.R`:

- `/getPersonCountsFilters`: replace `conceptId=0L` with `conceptIds=""`
  (required string); add `sexStratum=""`, `ageStratum=""`, `visitStratum=""`
  params parsed with the existing `.plumberParseIntCsv()` helper (same as
  `/getPersonCountsUpset` already does). Keep `yearsRange`.
- `/getPersonCountsUpset`: replace `conceptId=0L` with `conceptIds=""`;
  **remove** the `level` param entirely. Keep `yearsRange`, `sexStratum`,
  `ageStratum`, `visitStratum` as-is.
- Both: validate `nzchar(trimws(conceptIds))` up front → 400
  `"conceptIds must not be empty"`; let malformed-token errors from
  `.parsePersonCountsConceptIds()` surface through the existing
  `tryCatch(..., error = ...)` → 400 block, same pattern already used for
  every other endpoint.
- Update the `#*` roxygen-style plumber comments above each endpoint to
  describe the new `conceptIds` grammar.

---

## Part E — Tests

`tests/testthat/test-getPersonCountsUpset.R` and
`test-getPersonCountsFilters.R` — rewrite call sites (`conceptId = 317009L` →
`conceptIds = "317009SD"` or similar) and add new coverage:

- **Parsing/validation**: empty string, malformed token (`"abc"`,
  `"317009X"`, `"317009"` with no tag) each `expect_error()`.
- **Tag semantics** (Upset): `S` ⊆ `SD` (person_counts sum for the `S`-only
  region set is ≤ the `SD` set's); same for `M` ⊆ `MD`. Two tokens sharing a
  raw id but different tags (`2000403993M` and `2000403993MD`) produce
  distinct labels/rows, not a collision.
- **Filters cross-filtering**: given `sexStratum` fixed, the `age` breakdown
  changes accordingly (matches the same invariant the current
  "stratum filters narrow totals consistently" test checks, extended to
  assert `selected` is `TRUE` exactly on the rows matching the passed-in
  filter values and `FALSE` elsewhere, including all-`FALSE` for a dimension
  that had no filter passed).
- Keep the existing `yearsRange` inversion / bounded-sample invariant tests,
  updated for the new `conceptIds` signature.

---

## Part F — Docs

`development/STRUCTURE.md`:

- `getPersonCountsUpset()` / `getPersonCountsFilters()` getter table rows —
  update param descriptions (`conceptIds` grammar, dropped `level`/
  `codeCountsTable`).
- Output-shape sections — add the `selected` column to
  `getPersonCountsFilters()`'s table.
- `/getPersonCountsUpset` and `/getPersonCountsFilters` endpoint rows in the
  API table — update query params.

Run `devtools::document()` after signature/roxygen changes (regenerates
`man/getPersonCountsUpset.Rd` and `man/getPersonCountsFilters.Rd`).

---

## Verification

1. `devtools::document()` + `devtools::load_all()` — no errors.
2. `devtools::test(filter = "getPersonCounts")` against OnlyCounts-FinnGen —
   all new/updated tests pass.
3. Manual serve check: `tests/testmanual/manualtest-runApiServer.sh`, then
   - `GET /getPersonCountsUpset?conceptIds=317009SD,317009MD` — two distinct
     regions/labels back.
   - `GET /getPersonCountsFilters?conceptIds=317009SD&sexStratum=<id>` —
     `selected=TRUE` only on the matching sex row, `age`/`visit` rows
     recomputed without the sex filter applied to themselves.
   - Malformed `conceptIds` (e.g. `?conceptIds=abc`) → 400 with a clear error
     message.
4. `devtools::check()` clean (0 errors/0 warnings; pre-existing NOTEs OK).
