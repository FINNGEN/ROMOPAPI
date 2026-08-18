# Building UpSet buckets from HLL sketches

**Goal.** We have one **HLL sketch per set** (for any number of sets `N`). From
those we want the **UpSet buckets** — the count of elements in *exactly* each
combination of sets ("A only", "A and B but nothing else", …).

It's a 3-step pipeline, and every step is exact arithmetic:

```
HLL sketches ──merge──▶ union size of ANY subset ──┐
                                                   │ inclusion–exclusion
                        intersection of any subset ◀┘
                                                   │ Möbius
                        exclusive UpSet buckets   ◀┘
```

- **Step 1 — sketches → unions.** Merging the sketches of any subset `S` and
  reading its cardinality gives `U(S) = |⋃_{i∈S} Aᵢ|`. With `N` sketches you can
  produce the union of *every* subset — no extra storage.
- **Step 2 — unions → intersections.** Inclusion–exclusion.
- **Step 3 — intersections → buckets.** Möbius inversion.

Notation used throughout:

| Symbol | Meaning |
|---|---|
| `U(S)` | size of the **union** of the sets in `S` (measured by merging sketches) |
| `I(S)` | size of the **intersection** of the sets in `S` |
| `E(S)` | **UpSet bucket**: elements in *exactly* the sets in `S`, none outside |

---

## The whole method in two formulas

**Step 2 — intersection from unions** (self-inverse of inclusion–exclusion):

```
I(S) = Σ_{∅ ≠ T ⊆ S} (−1)^{|T|+1} · U(T)
```

**Step 3 — bucket from intersections** (Möbius; `F` = the full set of all `N`):

```
E(S) = Σ_{S ⊆ T ⊆ F} (−1)^{|T|−|S|} · I(T)
```

That's it. Everything below is just these two formulas on concrete numbers.

---

## Example · N = 2

**Sketches → unions** (Step 1):

```
U(A) = 100   U(B) = 90   U(A,B) = 160
```

**Unions → intersection** (Step 2):

```
I(A,B) = U(A) + U(B) − U(A,B) = 100 + 90 − 160 = 30
```

**Intersection → buckets** (Step 3):

| Bucket | Formula | Value |
|---|---|---|
| A only | `I(A) − I(A,B)` | 100 − 30 = **70** |
| B only | `I(B) − I(A,B)` | 90 − 30 = **60** |
| A ∩ B | `I(A,B)` | **30** |

Check: `70 + 60 + 30 = 160 = U(A,B)` ✓

---

## Example · N = 3

**Sketches → unions** (Step 1) — all 7 subset unions:

```
U(A)=100  U(B)=90  U(C)=80
U(A,B)=160  U(A,C)=150  U(B,C)=150
U(A,B,C)=195
```

**Unions → intersections** (Step 2):

```
I(A,B) = 100 + 90 − 160 = 30
I(A,C) = 100 + 80 − 150 = 30
I(B,C) =  90 + 80 − 150 = 20
I(A,B,C) = U(A)+U(B)+U(C) − U(A,B)−U(A,C)−U(B,C) + U(A,B,C)
         = 270 − 460 + 195 = 5
```

**Intersections → buckets** (Step 3):

| Bucket | Formula | Value |
|---|---|---|
| A only | `I(A) − I(A,B) − I(A,C) + I(A,B,C)` | 100 − 30 − 30 + 5 = **45** |
| B only | `I(B) − I(A,B) − I(B,C) + I(A,B,C)` | 90 − 30 − 20 + 5 = **45** |
| C only | `I(C) − I(A,C) − I(B,C) + I(A,B,C)` | 80 − 30 − 20 + 5 = **35** |
| A ∩ B only | `I(A,B) − I(A,B,C)` | 30 − 5 = **25** |
| A ∩ C only | `I(A,C) − I(A,B,C)` | 30 − 5 = **25** |
| B ∩ C only | `I(B,C) − I(A,B,C)` | 20 − 5 = **15** |
| A ∩ B ∩ C | `I(A,B,C)` | **5** |

Check: `45+45+35+25+25+15+5 = 195 = U(A,B,C)` ✓, and for A:
`45+25+25+5 = 100 = U(A)` ✓

---

## Example · N = 4

Sets A, B, C, D. (Numbers chosen symmetric just so the tables stay readable —
the method doesn't need symmetry.)

**Sketches → unions** (Step 1) — the 15 subset unions:

```
singles:  U(each) = 32
pairs:    U(each pair) = 54          (6 of them: AB AC AD BC BD CD)
triples:  U(each triple) = 69        (4 of them: ABC ABD ACD BCD)
quad:     U(A,B,C,D) = 79
```

**Unions → intersections** (Step 2):

```
pair:    I(A,B)   = 32 + 32 − 54 = 10
triple:  I(A,B,C) = 32·3 − 54·3 + 69 = 96 − 162 + 69 = 3
quad:    I(A,B,C,D) = 32·4 − 54·6 + 69·4 − 79
                    = 128 − 324 + 276 − 79 = 1
```

**Intersections → buckets** (Step 3):

| Bucket | Formula | Value |
|---|---|---|
| A only | `I(A) − ΣI(pair with A) + ΣI(triple with A) − I(quad)` = `32 − 3·10 + 3·3 − 1` | **10** |
| A ∩ B only | `I(A,B) − I(A,B,C) − I(A,B,D) + I(A,B,C,D)` = `10 − 3 − 3 + 1` | **5** |
| A ∩ B ∩ C only | `I(A,B,C) − I(A,B,C,D)` = `3 − 1` | **2** |
| A ∩ B ∩ C ∩ D | `I(A,B,C,D)` | **1** |

Every other bucket follows by symmetry (4 singles @ 10, 6 pairs @ 5, 4 triples
@ 2, 1 quad @ 1).

Check: `4·10 + 6·5 + 4·2 + 1·1 = 40+30+8+1 = 79 = U(A,B,C,D)` ✓

---

## General N

For `N` sets there are `2ᴺ − 1` non-empty subsets, and:

- **Merges available:** `2ᴺ − 1` — the union of every subset (Step 1), straight
  from the `N` sketches.
- **Buckets to fill:** `2ᴺ − 1` — the whole point.

Since Step 1 supplies *every* union, Steps 2–3 reconstruct *every* bucket
exactly:

```
I(S) = Σ_{∅ ≠ T ⊆ S} (−1)^{|T|+1} · U(T)                 (unions → intersections)

E(S) = Σ_{S ⊆ T ⊆ F} (−1)^{|T|−|S|} · I(T)               (intersections → buckets)
```

Both maps are invertible linear transforms, so the `2ᴺ − 1` subset unions carry
exactly the information of the `2ᴺ − 1` buckets — nothing missing, nothing
redundant. **This is why per-set sketches are enough for any `N`:** you never
have to precompute or store the combinations, you merge them on demand.

| `N` | subset unions = buckets `2ᴺ − 1` |
|---:|---:|
| 2 | 3 |
| 3 | 7 |
| 4 | 15 |
| 5 | 31 |
| 6 | 63 |
| 10 | 1023 |

---

## Two practical limits

Exact in principle; two costs in practice.

1. **Compute is exponential.** `2ᴺ − 1` merges, and the order-`k` sums have
   `2ᵏ − 1` terms. Fine to `N ≈ 5`; heavy beyond. In practice only evaluate the
   combinations that actually occur, or the top-k bars the plot will show.

2. **HLL is approximate, and inclusion–exclusion amplifies error.** Each sketch
   cardinality has relative error ≈ `1.04 / √(2^precision)` (precision 10 → ~3%;
   precision 15 → ~0.6%). A small bucket computed as a signed sum of large unions
   inherits an *absolute* error scaled to those large operands — so deep/small
   buckets get noisy and can even come out **negative**. Mitigate with the
   highest precision you can afford, clamp negatives to 0, and — for any bucket
   that must be exact — build a sketch for that intersection directly instead of
   reconstructing it by subtraction.
