# HLL++ merge (JS) — ROMOPAPI brainstorming

JS port of ZetaSketch HLL++ merge. Lets us combine BigQuery `HLL_COUNT.INIT`
sketches client-side without round-tripping to BQ.

## Files

- `zetasketch.proto` — minimal proto schema (outer `AggregatorStateProto` +
  inner `HyperLogLogPlusUniqueStateProto`). Field 112 of outer = inner
  payload (declared as `bytes` to sidestep protobufjs extension quirks).
  Inner field names corrected vs upstream (proto field 3 = normal precision,
  field 4 = sparse precision; upstream names are misleading).
- `mergeHll.js` — `loadProto()`, `mergeHllSketches(a, b)`, `estimate(blob)`,
  `toBase64()`. Dual-path merge:
  - both inputs sparse + same `sparsePrecision` → union packed entries,
    re-emit sparse (lossless).
  - else → promote both to dense register array, max-register merge, emit
    dense.
- `testAgainstBq.js` — round-trip test (see below).
- `package.json` — `@google-cloud/bigquery` + `protobufjs`.

## Setup

```sh
cd dev/brainstorming/hll
npm install
gcloud auth application-default login
export BQ_PROJECT=<your-project-id>
npm test
```

## The test (`testAgainstBq.js`)

Goal: prove our JS merge produces a sketch BigQuery accepts and that yields
the **same cardinality** as `HLL_COUNT.MERGE_PARTIAL` run inside BQ.

### Per-case flow

For each test case (defined by two integer ranges `[loA, hiA)` and
`[loB, hiB)`):

1. **Build two source sketches in BQ.**
   ```sql
   SELECT HLL_COUNT.INIT(x, 10) AS sketch
   FROM UNNEST(GENERATE_ARRAY(@lo, @hi - 1)) AS x
   ```
   Precision `p = 10` baked into SQL literal (BQ rejects it as a query
   parameter when used as the precision arg). Result returned as `Buffer`
   of serialized ZetaSketch proto bytes.

2. **Get BigQuery's reference merge.**
   ```sql
   SELECT HLL_COUNT.MERGE_PARTIAL(s) AS merged
   FROM UNNEST([FROM_BASE64(@a), FROM_BASE64(@b)]) AS s
   ```
   `bq_merged_card = HLL_COUNT.EXTRACT(merged)` — what BQ thinks the
   merged cardinality is. This is the **ground truth** the JS merge must
   match.

3. **Merge in JS.** Call `mergeHllSketches(sketchA, sketchB)` → returns a
   new sketch as `Uint8Array`.

4. **Ship JS-merged sketch back to BQ and extract.**
   ```sql
   SELECT HLL_COUNT.EXTRACT(FROM_BASE64(@s)) AS n
   ```
   `js_merged_card = HLL_COUNT.EXTRACT(jsMerged)`. If BQ rejects the bytes
   (`"Invalid input bytes"`), our proto encoding is broken.

5. **Strict assertion:**
   `js_merged_card === bq_merged_card`. Both numbers are produced by
   identical BQ extract logic (with full bias correction), so any mismatch
   reflects an error in JS merge, **not** BQ's estimation noise.

6. **Informational only:** `js_local_estimate = estimate(jsMerged)` runs
   our pure-JS estimator on the merged sketch, then logs the relative
   error vs. true distinct count. Drifts ~1–8 % at small cardinalities
   because we skipped the empirical bias-correction table — does not
   gate pass/fail.

### Cases

Numbers from latest run at `p = 10`. `bq_merged` and `js_merged(bq)` are
extracted via BQ — strict assertion is `bq_merged == js_merged(bq)`.
`js_local` is the pure-JS estimator (informational only, no bias table).

| Case               | A range       | B range          | true distinct | bq_merged | js_merged(bq) | bq vs true err % | js_local | js_local err % | Exercises                                                                  |
|--------------------|---------------|------------------|---------------|-----------|---------------|------------------|----------|----------------|-----------------------------------------------------------------------------|
| disjoint small     | `[0, 5)`      | `[100, 105)`     | 10            | 10        | 10            | 0.00 %           | 10       | 0.00 %         | Both inputs sparse, disjoint sets → sparse-union path                       |
| overlap small      | `[0, 50)`     | `[25, 75)`       | 75            | 75        | 75            | 0.00 %           | 69       | 8.00 %         | Both inputs sparse, overlap → tests dedup of identical sparse entries       |
| medium disjoint    | `[0, 2000)`   | `[10000, 12000)` | 4000          | 3999      | 3999          | 0.025 %          | 4002     | 0.05 %         | Inputs near sparse→dense crossover                                          |
| large disjoint     | `[0, 50000)`  | `[100000, 150000)` | 100000      | 98267     | 98267         | 1.73 %           | 98267    | 1.73 %         | Both inputs dense (2^p byte register array) → dense max-register path       |
| identical          | `[0, 1000)`   | `[0, 1000)`      | 1000          | 1004      | 1004          | 0.4 %            | 994      | 0.6 %          | Idempotency: merging a sketch with itself must equal the input              |

### Why two paths matter for the test

- **Dense path** (medium/large/identical): straightforward register-by-
  register max. Easy to get right, but only covers half the real-world
  inputs.
- **Sparse path** (disjoint small / overlap small): if we naively
  promoted both inputs to dense at `p = 10` and then merged, we'd lose
  3–6 entries to bucket collisions (2^15 sparse slots collapse into 2^10
  dense slots). The strict assertion would then fail by ~5–10 % on these
  cases. Hence the sparse-union branch in `mergeHllSketches`.

### Why pass-through to BQ EXTRACT (not local `estimate`)

Local `estimate()` lacks the empirical bias-correction table from the
HLL++ paper (~200 floats per precision in ZetaSketch's
`BiasCorrection.java`). Without it, small-cardinality estimates drift
~1–8 %. Comparing `js_merged_card` (via BQ) to `bq_merged_card` (via BQ)
cancels that gap — both numbers go through identical estimation logic,
so mismatch isolates merge correctness from estimator accuracy.

### Run output

```
--- disjoint small ---
  bq_merged_card     = 10
  js_merged_card(bq) = 10
  js_local_estimate  = 10
  true_distinct      = 10
  [PASS] ...
...
ALL PASS
```

Exit code 0 = all strict assertions passed. Exit code 1 = at least one
JS-merged sketch failed to match the BQ-merged reference.

## Caveats

- Hash compat: only merges sketches built with same hash family (e.g.
  both BQ-built over same `value_type`). Do not feed sketches built with
  custom hashing.
- No empirical bias-correction table → local `estimate()` drifts at
  small cardinalities. For exact parity, send the merged blob to BQ
  `HLL_COUNT.EXTRACT` (this is what the test does).
- Dense-output path emits `2^p` bytes regardless of nonzero density;
  not re-collapsed to sparse. Acceptable for query-time merge, less so
  for at-rest storage of small sketches.
- Type 112 (HLL++) only. No KLL / theta / variance sketches.
- BQ `@google-cloud/bigquery` BYTES query params double-encode; the test
  routes BYTES through STRING + SQL `FROM_BASE64()`.

See `mergeHll.js` top comment + earlier conversation `HANDOFF.md` thread
for full gotchas.
