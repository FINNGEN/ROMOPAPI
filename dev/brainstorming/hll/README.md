# HLL++ merge (JS) — ROMOPAPI brainstorming

JS port of ZetaSketch HLL++ merge. Lets us combine BigQuery `HLL_COUNT.INIT`
sketches client-side without round-tripping to BQ.

## How HyperLogLog works (one paragraph)

HLL estimates the number of **distinct** items in a set without storing the
items. Each item is hashed to a 64-bit number; the hash is split into two
parts: a *bucket index* (top `p` bits, where `m = 2^p` buckets) and a *tail*
(the remaining bits). For each bucket we keep only the position of the
leading 1-bit (`ρ`) of the tail across all items routed to that bucket — that
is, "the longest run of leading zeros I've seen + 1". Long zero-runs are rare,
so a long observed run implies many distinct items hashed into that bucket.
A harmonic mean across all `m` buckets, with empirical bias correction, gives
the cardinality estimate. Standard error ≈ `1.04 / √m`.

**HLL++ addition.** HyperLogLog**++** adds three things on top of plain HLL
(Heule, Nunkesser, Hall — *HyperLogLog in Practice*, 2013):

1. **64-bit hash** (vs HLL's 32-bit) — eliminates the >1 G cardinality
   correction.
2. **Sparse representation** at low cardinalities: instead of `m` registers
   most of which are 0, store only the non-zero `(bucketIdx, ρ)` pairs at
   higher *sparse* precision `p'` (typically `p + 5`). Saves space AND gives
   exact set-union on merge until promotion to dense kicks in.
3. **Empirical bias-correction table** — pre-computed per-precision tables of
   `(rawEstimate, observedBias)` learned from simulation. Subtracted from the
   raw estimate to cancel the small-cardinality bias of the alpha-corrected
   harmonic-mean formula. Plus a precision-specific threshold below which
   *linear counting* (`m · ln(m / zeros)`) is used instead.

Merging two HLL++ sketches is **lossless** in the sparse-only regime
(true set union of `(bucketIdx, ρ)` entries) and a register-wise `max` in
the dense regime — both commute and are associative, so order doesn't matter.

## Worked example: two patient groups

Suppose `cohort_A = {p1, p2, p3, p4, p5}` and `cohort_B = {p4, p5, p6, p7}`.
True distinct patients across both = 7 (p4 and p5 overlap).

```sql
-- 1. Build one sketch per cohort (server-side, BigQuery).
WITH a AS (SELECT HLL_COUNT.INIT(person_id, 10) AS s FROM cohort_A_table),
     b AS (SELECT HLL_COUNT.INIT(person_id, 10) AS s FROM cohort_B_table)

-- 2a. Combine + count in one step: returns 7.
SELECT HLL_COUNT.MERGE(s) AS distinct_patients
FROM (SELECT s FROM a UNION ALL SELECT s FROM b);

-- 2b. Or: combine into a re-usable sketch, count later.
SELECT HLL_COUNT.MERGE_PARTIAL(s) AS merged_sketch   -- BYTES, can be stored
FROM (SELECT s FROM a UNION ALL SELECT s FROM b);

SELECT HLL_COUNT.EXTRACT(merged_sketch);             -- returns 7
```

Client-side (this repo) does the same merge without going back to BigQuery:

```js
import { HLL_COUNT } from './mergeHll.js';

const sketchA = /* bytes from HLL_COUNT.INIT on cohort A */;
const sketchB = /* bytes from HLL_COUNT.INIT on cohort B */;

const merged = HLL_COUNT.MERGE_PARTIAL([sketchA, sketchB]); // Uint8Array
const distinctPatients = HLL_COUNT.EXTRACT(merged);          // 7
// or one shot:
const distinctPatients2 = HLL_COUNT.MERGE([sketchA, sketchB]); // 7
```

In R (`R/hllCount.R`):

```r
merged_b64 <- HLL_COUNT.MERGE_PARTIAL(c(sketchA_b64, sketchB_b64))
# Drop-in inside dplyr::summarise(node_hll_person_counts = HLL_COUNT.MERGE_PARTIAL(...))
```

`MERGE_PARTIAL` returns a sketch (mergeable further), `MERGE` returns the
final cardinality. Both are associative and commutative — fan-in order
doesn't matter. The same sketch can be merged into a third group later
without re-reading the underlying patient rows.

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
