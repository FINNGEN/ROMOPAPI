# Why sketches don't fit on-demand UpSet plots — query exact instead

## What we want to solve

Given a handful of concepts the user picks, draw an **UpSet plot** of how their
**patient sets overlap** — how many patients have concept A only, A∩B, A∩B∩C, etc.
The catch: it must be **on demand**. The user first **filters the strata**
(e.g. male, years 2000–2020), and the overlaps are recomputed for just that slice.
So we need per-concept patient-set **intersections**, on the fly, for any stratum
filter and any set of concepts (optionally including their descendants).

**Decision.** Abandon sketch-based overlap math. Build the plot by querying the
raw data at request time (by `concept_id` / `concept_id`-with-descendants,
filtered by stratum) and returning the exclusive regions directly.

## Why it's hard from what we store

- Source: `stratified_code_counts` — sets (`concept_id`) split by strata
  (sex, visit, year, age), with a per-row **HLL sketch** of distinct patients.
- Union per concept works today (merge HLL across the surviving strata).
- But UpSet needs **intersections / exclusive regions**, and the interesting
  bars are often tiny (privacy-floor, ~5–50 patients) — and HLL can't intersect.

## The sketch families and why each fails here

| Sketch | Good at | Drawback for UpSet |
|---|---|---|
| **HLL++** (what we use now) | distinct count, union; tiny, BigQuery-native | **No intersection.** Only via inclusion–exclusion, whose error compounds ~2ᵏ and returns negative counts. Keeps register maxima, so it *cannot* tell if the same person is in two concepts — the exact info an overlap needs. |
| **KMV / MinHash** | union, pairwise Jaccard; pure-SQL build | Sampling sketch: error on a region ≈ `√(N·m/K)`. To resolve a ~50-person bar inside a large union you need `K ≈ union size` → no saving over storing everyone. |
| **Theta** (DataSketches) | composable union ∩ diff, calibrated error | Same sampling limit on tiny bars; **needs a BQ UDF to build**; ~32× storage vs HLL. |
| **Bloom** | membership test | Not a distinct-count / overlap tool. |

Common wall: **any sketch that keeps only per-set summaries either loses the
cross-set person linkage (HLL) or samples it (KMV/Theta).** Sampling is fine for
big bars, hopeless for the small ones — which is exactly what we care about.

## The measured numbers that kill sketches

From `atlas-development-270609.finngen_omop_results_dev.stratified_code_counts`
(6.5M rows, 12,271 concepts, 800 MB HLL):

| Distinct persons | p50 | p90 | p99 | max |
|---|---|---|---|---|
| **per concept** (set size) | 1,059 | 27,218 | 182,834 | **495,661** |
| **per stratum** (one row) | 5 | 57 | 487 | 47,847 |

The hostile combination:

- **Sets are huge and skewed** — top concepts reach the whole ~500k population.
- **Bars are tiny** — strata median 5 persons; UpSet regions sit at the privacy
  floor.

A ~50-person bar inside a 180k-person union is the worst case for sampling: to
get it right you'd sample nearly the entire union, so a fixed-`K` sketch buys
nothing. HLL inclusion–exclusion is worse (noise + negatives). **No sketch gives
accurate small bars here.**

## What fits instead: query exact

Strata are small (median 5 persons/row), so exact overlap is cheap:

- Compute the UpSet **in BigQuery** at request time: filter strata, restrict to
  the selected concepts (optionally with descendants), `GROUP BY` each person's
  membership bitmask over the chosen concepts, return region counts. Only the
  small result crosses the wire.
- Needs a person×concept×stratum bridge in the results schema (~252M rows,
  **~2 GB physical** clustered by `concept_id` — same order as today's HLL
  table). Built from the **raw CDM** (HLL can't be reversed to person ids).
- Keep HLL for what it's good at (union-per-group cardinality); add exact only
  for the overlap feature.

**Caveats:** the bridge is pseudonymous person-level data in the results schema
(governance) and small bars are small-cell counts (apply the usual suppression).

See `alternatives.md` (sketch comparison) and `upset_plot_from_sets.md`
(inclusion–exclusion math) for the long versions.
