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

## Conceptual walk-through: two patient cohorts

Use `m = 4` buckets (i.e. `p = 2`, top 2 bits = bucket index, remaining bits =
the *tail* used to derive `ρ`). Toy hashes so the math fits on a page.

**Cohort A = {p1, p2, p3, p4, p5}.** Each patient id is hashed; the hash
splits into `(bucket, tail)`; `ρ` is `(# leading zeros in tail) + 1`.

| patient | hash (binary) | bucket | tail      | ρ |
|---------|---------------|--------|-----------|---|
| p1      | `01 011010`   | 1      | `011010`  | 2 |
| p2      | `11 000110`   | 3      | `000110`  | 4 |
| p3      | `00 100000`   | 0      | `100000`  | 1 |
| p4      | `10 001100`   | 2      | `001100`  | 3 |
| p5      | `01 110000`   | 1      | `110000`  | 1 |

**`HLL_COUNT.INIT(cohort_A)`** — per bucket, keep `max(ρ)` across all
patients routed to that bucket. Result is the *register array* (the sketch):

```
              b0          b1            b2       b3
patients →    p3 (ρ=1)    p1 (ρ=2)      p4 (ρ=3) p2 (ρ=4)
                          p5 (ρ=1)
              ─────────   ─────────     ───────  ───────
max ρ      =  1           max(2,1)=2    3        4

sketch_A   = [ 1   ,      2          ,  3     ,  4 ]
```

Buckets b0, b2, b3 each saw exactly one patient. Bucket b1 saw two (p1, p5),
so the register stores `max(2, 1) = 2`. p3 with `ρ = 1` is the only patient
in b0; p4 in b2; p2 in b3.

**Cohort B = {p4, p5, p6, p7}.** p4 and p5 reuse the hashes above; p6 hashes
to `10 000010` (bucket 2, ρ=5); p7 hashes to `00 000011` (bucket 0, ρ=6).

| patient | hash (binary) | bucket | tail      | ρ |
|---------|---------------|--------|-----------|---|
| p4      | `10 001100`   | 2      | `001100`  | 3 |
| p5      | `01 110000`   | 1      | `110000`  | 1 |
| p6      | `10 000010`   | 2      | `000010`  | 5 |
| p7      | `00 000011`   | 0      | `000011`  | 6 |

```
              b0          b1          b2            b3
patients →    p7 (ρ=6)    p5 (ρ=1)    p4 (ρ=3)      (none)
                                      p6 (ρ=5)
              ─────────   ─────────   ───────────   ───────
max ρ      =  6           1           max(3,5)=5    0

sketch_B   = [ 6   ,      1       ,   5         ,   0 ]
```

Bucket b3 received no patient, so its register stays `0` (initial value).

**`HLL_COUNT.MERGE_PARTIAL([sketch_A, sketch_B])`** — element-wise
`max` of the two register arrays. That's it:

```
sketch_A    = [ 1 , 2 , 3 , 4 ]
sketch_B    = [ 6 , 1 , 5 , 0 ]
                ↓   ↓   ↓   ↓
merged      = [ 6 , 2 , 5 , 4 ]   ← register-wise max
```

Crucially, `merged` is **bit-identical** to what `HLL_COUNT.INIT` would have
produced if you had run it directly on the union `{p1..p7}`. Max is
associative + commutative → order of merges doesn't matter, duplicates
(p4, p5 seen twice) contribute nothing extra.

**`HLL_COUNT.EXTRACT(merged)`** — recover the cardinality estimate from the
register array via:

```
                α_m · m²
estimate  ≈  ────────────────
              Σ_i  2^(−ρ_i)
```

where `α_m` is a precision-dependent normalization constant (`≈ 0.7213 /
(1 + 1.079/m)` for large `m`; tabulated for small `m`).

**Why this formula works (intuition).**

A random hash tail starting with `ρ` zeros is rare: probability `1/2^ρ`.
E.g. tail `000000` has only a `1/64` chance. So if a bucket's register
shows ρ = 6, you most likely had to draw `~2^6 = 64` hashes into it
before one happened to land on a tail that long. **The register value
tells you, in log scale, roughly how many items went through that bucket.**

For a *single* bucket, that's a noisy estimate — random variation could
push ρ up or down by a couple of bits. But the same lucky deviation
showing up in **all `m` buckets simultaneously** is exponentially
unlikely. Combining the `m` independent probes (one harmonic-mean fusion
across buckets) cancels the per-bucket noise and the global estimate
converges to the true distinct count.

Mechanically: each of the `m` buckets sees ≈ `n / m` items, so its
register reads `2^ρ ≈ n / m`. Globalize → `estimate ≈ m · 2^ρ̄`. Because
`ρ` is a max (heavy-tailed), use the **harmonic mean** of `2^ρ_i` over
buckets — robust to a single lucky long run:

```
HMean(2^ρ_i)  =  m / Σ_i 2^(−ρ_i)
```

Plug in → `estimate ≈ m · HMean = m² / Σ 2^(−ρ_i)`. `α_m` out front is a
small empirical correction for residual bias.

Variance shrinks as `1/√m` (each bucket = an independent probe), so the
standard error of the estimate is `1.04 / √m` — that's why more buckets
(higher precision `p`) gives tighter estimates.

**Apply to `merged = [6, 2, 5, 4]` (m = 4):**

```
Σ 2^(−ρ_i) = 2^−6 + 2^−2 + 2^−5 + 2^−4
           = 1/64  + 1/4  + 1/32 + 1/16
           = 0.015625 + 0.25 + 0.03125 + 0.0625
           = 0.359375

α_m  ≈ 0.7213 / (1 + 1.079/4)
     ≈ 0.568

estimate ≈ (0.568 · 4²) / 0.359375
         ≈ 9.088     / 0.359375
         ≈ 25
```

Truth was 7. The toy `m = 4` is below the HLL++ supported range (real use is
`m ≥ 16`, i.e. `p ≥ 4`), and at this scale the raw formula is severely
biased upward. That's exactly why HLL++ adds two corrections on top:

- **Linear counting** when many registers are 0 (here all 4 are non-zero, so
  it doesn't apply). Formula: `m · ln(m / #zeros)`.
- **Empirical bias subtraction**: lookup `(rawEstimate → bias)` in a
  per-precision table (6-nearest-neighbour interpolation), then return
  `round(rawEstimate − bias)`.

With real `m = 1024` (p = 10) and the bias table applied, the same kind of
merge converges to the true distinct count within the standard error
`1.04 / √m ≈ 3.25 %`.

`HLL_COUNT.MERGE(...)` is just `EXTRACT(MERGE_PARTIAL(...))` rolled into one
step — use `MERGE_PARTIAL` when you want to keep the sketch around for
further aggregation, `MERGE` when you only need the final count.

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
