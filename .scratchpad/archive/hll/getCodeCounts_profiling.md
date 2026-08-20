# Profiling `getCodeCounts()` — conceptId 21601855

**Date:** 2026-08-17
**Tool:** `profvis` (flame graph) + `Rprof` (line/function breakdown)
**Database:** `OnlyCounts-FinnGen` SQLite fixture
(`inst/testdata/data/FinnGenR13_countsOnly.sqlite`, precomputed counts, real HLL data)
**R:** 4.6.1

## What was measured

`getCodeCounts(CDMdbHandler, conceptId = 21601855)` — a large hierarchy:

- **589** descendant concepts with counts
- **238,122** rows returned by the stratified-counts fetch

## Timing

| Scenario | Elapsed |
|---|---|
| Cold (incl. one-time `getConceptsWithCodeCounts` catalogue load) | **72.2 s** |
| Warm (catalogue cached — real `/getCodeCounts` request), 5 runs | **72.0 s** avg (71.7–72.7) |

Cold ≈ warm ⇒ the memoised concept-catalogue load is **not** the bottleneck.
The per-request work dominates.

Rprof sampled **45.2 s** of in-R work out of the ~72 s wall. The remaining
~27 s is outside R's sampler (JDBC/DBI query + fetch, GC).

## Where the time goes

### 1. R-side HyperLogLog merge — ~43 s (96% of sampled R time) — THE hotspot

Non-BigQuery branch in `R/getCodeCounts.R:248-254`, which merges HLL person-count
sketches per stratum group inside `dplyr::summarise`:

```r
node_hll_person_counts = HLL_COUNT.MERGE_PARTIAL(node_hll_person_counts)
```

The SQLite fixture ships real HLL sketches (base64), so this fires the pure-R port
in `R/hllCount.R`. Almost all cost is interpreted-R varint codec churn, once per
stratum group across the 238k rows:

| Function (`R/hllCount.R`) | Self time | Role |
|---|---|---|
| `.readVarint` (`hllCount.R:17`) | 12.5 s (28%) | varint decode, byte-at-a-time `repeat` loop |
| `.writeVarint` (`hllCount.R:31`) | 6.9 s (15%) | varint encode |
| `.decodeSparseEntries` (`hllCount.R:120`) | 20.5 s total (45%) | decode sparse sketch |
| `.encodeSparseEntries` (`hllCount.R:136`) | 11.2 s total (25%) | re-encode sparse sketch |
| `bitwAnd`/`bitwOr`/`bitwShift*` | ~6 s | bit ops inside the codecs |

Call stack (from Rprof `by.total`): `getCodeCounts` → `dplyr::summarise` →
`HLL_COUNT.MERGE_PARTIAL` (43.3 s, 95.9%) → `Reduce` → `.decodeSparseEntries` /
`.encodeSparseEntries` → `.readVarint` / `.writeVarint`.

### 2. SQL fetch — most of the ~27 s Rprof can't see

`SELECT … FROM stratified_code_counts WHERE concept_id IN (…) OR maps_to_concept_id IN (…)`
at `R/getCodeCounts.R:169-182` returns 238k rows over DatabaseConnector/JDBC (Java,
invisible to Rprof) plus DBI marshalling.

### 3. Everything else — negligible (<1 s combined)

Family-tree `WITH` query, `.familyTreeToAncestorTable`, descendant-rollup joins.

## Bottom line / optimization candidates

The dominant cost is **merging HLL sketches in interpreted R** for a concept with a
huge stratum count. In rough order of payoff:

1. **Avoid the R-side HLL merge on SQLite.** Counts are precomputed; the per-request
   `HLL_COUNT.MERGE_PARTIAL` over 238k rows is the whole cost. Pre-aggregate person
   counts at build time, or push the merge into SQL.
2. **Vectorize / compile `R/hllCount.R`.** `.readVarint`/`.writeVarint` are scalar
   byte loops called millions of times; a C/Rcpp (or vectorized) rewrite would cut
   the ~43 s dramatically.
3. **Shrink the fetch.** 238k rows for one concept suggests the `IN (…) OR maps_to…`
   predicate pulls a very wide set; aggregating in SQL before returning reduces both
   transfer and downstream R work.

## Reproduction

Scripts (session scratchpad, not committed):
- `profile_getCodeCounts.R` — timing + profvis (20-call loop) + Rprof
- `rprof_breakdown.R` — single-call Rprof line/function breakdown

Artifacts:
- `profvis_getCodeCounts.html` — interactive flame graph (109 MB; 20-call loop @ 5 ms)
- `getCodeCounts_1call.Rprof`, `rprof_summary.rds` — single-call breakdown

Setup: build the `OnlyCounts-FinnGen` handler via
`HadesExtras_readAndParseYaml(..., pathToFinnGenCountsSqlite = helper_FinnGen_getDatabaseFileCounts())`
→ `databasesConfig$FC$cohortTableHandler` →
`HadesExtras_createCDMdbHandlerFromList(...)`, then call
`getCodeCounts(CDMdbHandler, conceptId = 21601855)`.
