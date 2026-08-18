# UpSet buckets from per-set HLL sketches

**Premise.** We hold **one HLL sketch per set**, for any number of sets `N`.
From those sketches we can build an UpSet plot — the exclusive buckets ("A only",
"A ∩ B but nothing else", …) — in two steps:

```
 HLL sketches ──①──► every subset UNION ──②──► every UpSet BUCKET
   (N of them)         (2ᴺ − 1 of them)          (2ᴺ − 1 of them)
```

Both steps are exact in arithmetic; the only real cost is compute and HLL
approximation error (see §5).

---

## The two steps

### ① Sketches → unions (merge)

An HLL sketch **merges to a union**. For any subset `S` of sets:

```
U(S) = |⋃_{i∈S} Aᵢ| = cardinality( merge of the sketches for i ∈ S )
```

No extra storage — just the `N` base sketches, merged on demand. This gives uss
the count of **every** subset union.

### ② Unions → buckets (inclusion–exclusion)

Two sub-steps. First recover each **intersection** from the unions (the
union↔intersection transform is self-inverse):

```
I(S) = |⋂_{i∈S} Aᵢ| = Σ_{∅ ≠ T ⊆ S} (−1)^{|T|+1} · U(T)
```

Then turn intersections into **exclusive buckets** — elements in exactly the
sets of `S` and none of the others (this is what an UpSet bar is) — by Möbius
inversion over the supersets of `S`:

```
E(S) = Σ_{T ⊇ S} (−1)^{|T|−|S|} · I(T)
```

That's the whole method. The examples below just expand these two formulas for
`N = 2, 3, 4`.

---

## Example · N = 2

**Unions measured** (from the sketches):

```
U(A) = 100    U(B) = 70    U(AB) = 150
```

**Intersection:**

```
I(AB) = U(A) + U(B) − U(AB) = 100 + 70 − 150 = 20
```

**Buckets:**

```
E(A)  = I(A) − I(AB) = 100 − 20 = 80     (A only)
E(B)  = I(B) − I(AB) =  70 − 20 = 50     (B only)
E(AB) = I(AB)        =            20     (A ∩ B)
```

Check: `80 + 50 + 20 = 150 = U(AB)` ✓

---

## Example · N = 3

**Unions measured:**

```
U(A)=100  U(B)=90  U(C)=80
U(AB)=160  U(AC)=150  U(BC)=150
U(ABC)=195
```

**Intersections** (transform ①→I):

```
I(AB) = 100+90−160 = 30
I(AC) = 100+80−150 = 30
I(BC) =  90+80−150 = 20
I(ABC)= U(A)+U(B)+U(C) − U(AB)−U(AC)−U(BC) + U(ABC)
      = 270 − 460 + 195 = 5
```

**Buckets** (Möbius):

| Bucket | Formula | Value |
|---|---|---|
| A only | `I(A) − I(AB) − I(AC) + I(ABC)` = 100−30−30+5 | **45** |
| B only | `I(B) − I(AB) − I(BC) + I(ABC)` =  90−30−20+5 | **45** |
| C only | `I(C) − I(AC) − I(BC) + I(ABC)` =  80−30−20+5 | **35** |
| A ∩ B only | `I(AB) − I(ABC)` = 30−5 | **25** |
| A ∩ C only | `I(AC) − I(ABC)` = 30−5 | **25** |
| B ∩ C only | `I(BC) − I(ABC)` = 20−5 | **15** |
| A ∩ B ∩ C | `I(ABC)` | **5** |

Check: `45+45+35+25+25+15+5 = 195 = U(ABC)` ✓

---

## Example · N = 4

To keep the arithmetic readable, use a **symmetric** dataset where a region's
size depends only on its order (how many sets it belongs to): every
single-only = 10, every pair-only = 4, every triple-only = 2, all-four = 1. By
symmetry all subsets of the same order share one union value, so we only compute
four distinct numbers.

**Unions measured** (all 15 subsets, grouped by order):

```
4 singles  : U = 29    (e.g. |A|)
6 pairs    : U = 49    (e.g. |A∪B|)
4 triples  : U = 63    (e.g. |A∪B∪C|)
1 quad     : U = 73    (|A∪B∪C∪D| = the total)
```

**Intersections** (transform ①→I — signs `+ − + −` by term order):

```
I(AB)   = 2·29 − 49                       = 9        (all 6 pairs)
I(ABC)  = 3·29 − 3·49 + 63                = 3        (all 4 triples)
I(ABCD) = 4·29 − 6·49 + 4·63 − 73         = 1
```

**Buckets** (Möbius — a 4-set bucket sums over supersets up to order 4):

```
E(ABCD) = I(ABCD)                                  = 1
E(ABC)  = I(ABC) − I(ABCD)             = 3 − 1      = 2   (triple, not the 4th)
E(AB)   = I(AB) − I(ABC) − I(ABD) + I(ABCD)
        = 9 − 3 − 3 + 1                             = 4   (pair only)
E(A)    = I(A) − [I(AB)+I(AC)+I(AD)]
              + [I(ABC)+I(ABD)+I(ACD)] − I(ABCD)
        = 29 − 3·9 + 3·3 − 1                        = 10  (A only)
```

Each recovers its ground-truth region (10, 4, 2, 1). All 15 buckets sum to
`4·10 + 6·4 + 4·2 + 1 = 73 = U(ABCD)` ✓ (verified by script).

Notice the **sign pattern** of `E(S)`: alternating by how many extra sets a
superset adds — `+` for `S` itself, `−` for one extra set, `+` for two, `−` for
three, … This is the general rule, next.

---

## General `N`

For any `N`, with `S` a subset of the `N` sets:

**① Union of a subset** — measured directly by merging that subset's sketches:

```
U(S) = |⋃_{i∈S} Aᵢ| = card( merge sketches i ∈ S )
```

**② Intersection of a subset** — from the unions of its own subsets:

```
I(S) = Σ_{∅ ≠ T ⊆ S} (−1)^{|T|+1} · U(T)
```

**③ Exclusive bucket for a subset** — from the intersections of its supersets:

```
E(S) = Σ_{S ⊆ T ⊆ {1..N}} (−1)^{|T|−|S|} · I(T)
```

`E(S)` is the UpSet bar "exactly the sets in `S`". Composing ② and ③ maps the
`2ᴺ − 1` subset unions to the `2ᴺ − 1` buckets by an invertible linear
transform — so the information is always exactly sufficient, for **any `N`**.

Sanity properties that always hold:

- Buckets are non-negative and sum to the grand union `U({1..N})`.
- The buckets touching set `i` sum back to `|Aᵢ|`.

---

## §5 · Cost and accuracy

Two practical limits — the reconstruction is exact only in exact arithmetic:

1. **Exponential work.** There are `2ᴺ − 1` subset unions and the same number of
   buckets; an order-`k` intersection sums `2ᵏ − 1` union terms. Comfortable to
   `N ≈ 5`, heavy by `N ≈ 20`. In practice you evaluate only the combinations
   that actually occur, or the top-k bars UpSet will draw — not the full power
   set.

2. **HLL is approximate, and inclusion–exclusion amplifies the error.** Each
   cardinality carries a relative standard error ≈ `1.04 / √(2^precision)`
   (precision 10 → ~3%; precision 15 → ~0.6%). A small bucket obtained as a
   signed sum of large unions inherits an *absolute* error scaled to those large
   operands — so small / high-order buckets get noisy and can even come out
   **negative**. Mitigations: use the highest sketch precision you can afford,
   clamp negatives to 0, treat deep buckets as noise-dominated, and where a
   bucket must be exact, build a sketch for that intersection directly instead of
   reconstructing it by subtraction.
