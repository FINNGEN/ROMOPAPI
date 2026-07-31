# Alternatives to HLL++ for set algebra

HLL++ (used by BigQuery via `HLL_COUNT.*`) is the right tool for **distinct
counts** and **union** but does NOT support intersection or difference
natively. Below: three sketch families that handle multi-set operations,
with a precision comparison vs HLL++ at `p = 10` (the BQ default in
ROMOPAPI).

## MinHash / KMV (K-Minimum-Values)

**Idea.** Hash every item, keep the `K` smallest hash values per set. That
sample IS the sketch.

```
set A = {x1, x2, x3, ...}    →  hash each, keep K smallest  →  KMV_A
set B = {y1, y2, ...}                                       →  KMV_B
```

**Operations.**

- **Cardinality.** `K` uniformly random hashes in `[0, 1)` → expected max
  is `~K / n`. Invert: `|A| ≈ K / max(KMV_A)`.
- **Union.** Merge the two K-sets, keep the `K` smallest of the combined
  `2K` → KMV of `A ∪ B`. Associative, lossless if `K` is large enough.
- **Intersection.** A hash value lies in `A ∩ B` iff it's in BOTH sketches.
  So:
  ```
  Jaccard(A, B) = |A ∩ B| / |A ∪ B|
                ≈ |KMV_A ∩ KMV_B| / K
  |A ∩ B|      = Jaccard · |A ∪ B|
  ```

**Why it works.** The K smallest hashes are an unbiased random *sample* of
the set. Sampling-based set ops give calibrated, unbiased estimates without
inclusion-exclusion noise blowup.

**Size.** `K · 8 bytes` (one int64 per slot). Typical `K ∈ [256, 8192]`.

**Standard error.**

- Cardinality: `≈ 1 / √(K − 2)`.
- Jaccard: `≈ √(J · (1 − J) / K)` where `J = Jaccard(A, B)`.

## Theta sketches (Apache DataSketches)

**Idea.** Generalization of KMV. Store all hashes **below a moving
threshold** `θ ∈ (0, 1)` instead of "the K smallest". Threshold tightens as
the sketch fills. Cleaner algebra than raw KMV.

**Operations** (all return a new theta sketch, never lose composability):

- **Union.** Combine samples, retain entries below the smaller of the two θ.
- **Intersection.** Keep only hashes present in BOTH samples that are below
  the common θ. Output sketch has its own valid θ.
- **A − B (difference).** Hashes in A's sample not in B's, under common θ.
- **Cardinality.** Same KMV-style invert: `n ≈ size(sample) / θ`.

Compositional: chain union/intersect/diff repeatedly without bias blowup.
Each operation carries calibrated standard-error bounds.

**Size.** Roughly `K · 8 bytes` like KMV (configurable `K`).

**Standard error.**

- Cardinality / set op: `≈ 1 / √K`.
- After intersections of `k` sets, error grows roughly as `√k / √K` — much
  better than HLL inclusion-exclusion which compounds as `2^k`.

**Ecosystem.** Apache DataSketches: Java, C++, Python. No native BigQuery
support but UDFs exist.

## Bloom filters

**Idea.** Bit array of size `m` + `k` hash functions. Insert by setting `k`
bits. Test by checking all `k` bits.

**Operations.**

- **Membership.** All `k` bits set ⇒ "probably present" (false-positive
  rate ≈ `(1 − e^(−kn/m))^k`); any bit unset ⇒ "definitely absent".
- **Union (`OR`).** Bitwise OR of two arrays. Lossless.
- **Intersection (`AND`).** Bitwise AND ≈ intersection filter, but
  false-positive rate accumulates (you get more FPs after AND than for
  either input).
- **Cardinality.** Estimate from #set bits: `n̂ ≈ −(m/k) · ln(1 − bits_set/m)`.
  Worse than HLL for the same memory.

**Strength.** Fast membership lookup ("is patient `p` in cohort A?"). Not a
distinct-count tool. Use Bloom for filtering pipelines, NOT for UpSet/Jaccard.

**Size.** Depends on target FPR. E.g. 1 M items @ FPR=1 % → ~1.2 MB.

---

## Precision comparison vs HLL++ (p = 10)

HLL++ at `p = 10`: 1024 registers, std err `1.04/√1024 ≈ 3.25%`, ~1 KB
dense sketch.

KMV / Theta picked at `K = 1024` for a like-for-like size comparison
(8 KB raw — 8× HLL). Theta at `K = 4096` (~32 KB) included as a
better-precision option.

### Single-set cardinality `|A|` (relative std err)

| set size   | HLL++ p=10 | KMV K=1024 | Theta K=4096 | Bloom (8 KB)             |
|------------|------------|------------|--------------|---------------------------|
| 100        | linear cnt → ~0 % | ~3.1 %     | ~1.6 %       | ~7 % (high FPR)           |
| 1 000      | ~3.25 %    | ~3.1 %     | ~1.6 %       | ~6 %                      |
| 10 000     | ~3.25 %    | ~3.1 %     | ~1.6 %       | ~10 % (bits saturating)   |
| 100 000    | ~3.25 %    | ~3.1 %     | ~1.6 %       | breaks down (FPR → 100%)  |
| 1 000 000  | ~3.25 %    | ~3.1 %     | ~1.6 %       | unusable                  |

### Union `|A ∪ B|` (relative std err)

Same as single-set for HLL/KMV/Theta — `MERGE` is lossless register-wise
max / sample merge.

| set size (each) | HLL++ p=10 | KMV K=1024 | Theta K=4096 |
|-----------------|------------|------------|--------------|
| 1 000           | ~3.25 %    | ~3.1 %     | ~1.6 %       |
| 100 000         | ~3.25 %    | ~3.1 %     | ~1.6 %       |
| 1 000 000       | ~3.25 %    | ~3.1 %     | ~1.6 %       |

### Intersection `|A ∩ B|` (relative std err)

Where the families diverge sharply. Numbers assume two sets of equal size
with overlap fraction (Jaccard).

| sets each | overlap (Jaccard) | HLL++ p=10 (via incl-excl) | KMV K=1024            | Theta K=4096 |
|-----------|-------------------|----------------------------|------------------------|--------------|
| 1 000     | 0.50              | ~10 %                      | ~1.6 %                 | ~0.8 %       |
| 1 000     | 0.10              | ~50 %                      | ~3.0 %                 | ~1.5 %       |
| 1 000     | 0.01              | **>500 %** (often negative)| ~9.8 %                 | ~4.9 %       |
| 100 000   | 0.50              | ~6.5 %                     | ~1.6 %                 | ~0.8 %       |
| 100 000   | 0.01              | ~325 %                     | ~9.8 %                 | ~4.9 %       |
| 1 000 000 | 0.001             | unusable                   | ~31 %                  | ~15 %        |

### Three-set union/intersection (UpSet on `k = 3`)

| operation                | HLL++ p=10                         | Theta K=4096              |
|--------------------------|------------------------------------|----------------------------|
| `|A ∪ B ∪ C|`            | ~3.25 %                            | ~1.6 %                     |
| `|A ∩ B|, |A ∩ C|, |B ∩ C|` | each ~10–500 % depending on overlap| each ~0.8–5 %              |
| `|A ∩ B ∩ C|`            | **catastrophic** (8-term sum, signs)| ~1–10 % (one op)           |

Beyond `k = 4`, HLL via inclusion-exclusion is essentially unusable for
intersections.

### Storage and merge cost

| sketch       | size at p=10 / K       | merge complexity        | BQ native?          |
|--------------|------------------------|--------------------------|---------------------|
| HLL++ p=10   | ~1 KB dense, sparse smaller | `O(m)` max per register | yes (`HLL_COUNT.*`) |
| KMV K=1024   | 8 KB                   | `O(K)` merge-keep-K-smallest | UDF / client-side |
| Theta K=4096 | 32 KB                  | `O(K)` per op            | UDF / DataSketches  |
| Bloom 8 KB   | 8 KB                   | `O(m)` bitwise OR        | UDF only            |

---

## Why not switch ROMOPAPI to Theta?

Theta beats HLL++ on intersection precision and multi-set algebra. So why
stay on HLL++? Drawbacks of switching:

1. **Storage blows up ~32×.** Theta `K=4096` ≈ 32 KB per sketch; HLL++
   p=10 ≈ 1 KB dense (and far smaller for low-cardinality sparse mode).
   A `stratified_code_counts` table with millions of rows holding sketches
   pays this cost on every row.

2. **No BigQuery-native ops.** HLL has `HLL_COUNT.{INIT,MERGE,
   MERGE_PARTIAL,EXTRACT}` built in. Theta needs one of:
   - A JS UDF in BQ (slow per-row JS overhead).
   - Apache DataSketches Java UDF (works, but ops overhead — JAR upload,
     IAM, lifecycle).
   - Pulling sketches to R/JS and doing ops client-side (loses server-side
     aggregation).
   ROMOPAPI's current pipeline builds counts in BQ and pulls blobs to R —
   either route is significantly slower than `HLL_COUNT.MERGE_PARTIAL`.

3. **Same-bytes cardinality precision is worse.** Apples-to-apples per
   memory budget:

   | sketch size  | tool             | cardinality std err |
   |--------------|------------------|---------------------|
   | ~1 KB        | HLL++ p=10       | 3.25 %              |
   | ~1 KB (K=128)| Theta            | ~8.8 %              |
   | ~8 KB (K=1024) | Theta          | ~3.1 %              |
   | ~8 KB        | HLL++ p=13       | 1.15 %              |

   For pure `|A|` and `|A ∪ B|`, HLL++ is **better per byte**.

4. **No sparse representation.** HLL++'s sparse mode gives free precision
   for low cardinalities (hundreds–thousands distinct → exact set merge,
   sketch shrinks to a fraction of a KB). Theta is fixed-size regardless
   of fill.

5. **Empirical bias correction.** HLL++ ships with a calibrated 6-NN bias
   table per precision (we ported it from ZetaSketch). Theta is *naturally*
   unbiased and doesn't need one, but its raw variance per byte is higher
   than HLL+bias for cardinality alone.

6. **Tiny-intersection bias.** Theta intersection can hit a "lower bound"
   collapse when overlap is so small that no sampled hash appears in both —
   estimate goes to 0 even when the true intersection is small-but-nonzero.

7. **R / JS ecosystem maturity.** Apache DataSketches has first-class
   Java/C++/Python. R has the `datasketches` CRAN package (smaller
   community). JS bindings exist but are less battle-tested than HLL ports.

8. **Cross-precision composition.** HLL++ merges sketches of different `p`
   cleanly (auto-downgrade). Theta merge across mismatched `K` works but
   has subtle pitfalls around effective `K` and threshold alignment.

9. **Migration cost.** Switching means: (a) rewrite
   `stratified_code_counts` schema (BYTES col grows ~32×); (b) replace
   `HLL_COUNT.MERGE_PARTIAL` with a UDF; (c) extend the R/JS port to
   parse the Theta sketch wire format too.

**Verdict for ROMOPAPI.** Current use case = union + cardinality on
`node_hll_person_counts` (single cohort or rollup across descendants). HLL++
wins on storage, BQ-native ops, and per-byte cardinality precision. Theta
only pays off once intersections / UpSet / multi-set algebra become
first-class features. Don't migrate unless intersection is the actual
driver.

## Summary

| what you need                                | best tool         |
|----------------------------------------------|-------------------|
| `|A|`, `|A ∪ B|`, low memory, BQ-native      | **HLL++**          |
| `|A ∩ B|`, Jaccard, 2-set algebra            | **KMV / MinHash**  |
| Multi-set algebra (`k ≥ 3`, union ∩ diff)    | **Theta**          |
| Element lookup "is p in cohort?"             | **Bloom**          |

HLL++ remains optimal for ROMOPAPI's main use case (`node_hll_person_counts`
of one cohort or the union of descendants). If UpSet-style intersections
across multiple cohorts become a feature, switch to Theta sketches —
similar engineering ergonomics, dramatically better intersection precision.
