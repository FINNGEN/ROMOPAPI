# HLL Storage Options in BigQuery (p = 10)

Five options for storing a HyperLogLog sketch of `person_id` per stratum row in
BigQuery. Goal: support a `node_person_hll` column on `stratified_code_counts`
that the API can return as a list of `(bucket, n)` pairs.

All numbers assume `p = 10` (1024 buckets), 48-bit hash, `n` (leading-zero count + 1)
fits in 6 bits.

---

## A. Sparse struct array

**Schema**

```sql
node_person_hll ARRAY<STRUCT<bucket INT64, n INT64>>
```

**Example row** (5 persons, 4 distinct buckets hit):

```
[{bucket:42, n:3}, {bucket:128, n:1}, {bucket:723, n:5}, {bucket:900, n:2}]
```

Only stores nonzero buckets. Empty buckets (`n = 0`) omitted.

**Build**

```sql
WITH hashed AS (
  SELECT strata..., person_id,
         FARM_FINGERPRINT(CAST(person_id AS STRING)) AS h
  FROM events
),
b AS (
  SELECT strata...,
    (h & 0xFFC0000000) >> 38 AS bucket,                -- top 10 of low 48
    suffix_leading_zeros(h & 0x3FFFFFFFFF) + 1 AS n    -- LZ in 38-bit suffix
  FROM hashed
),
m AS (
  SELECT strata..., bucket, MAX(n) AS n
  FROM b
  GROUP BY strata..., bucket
)
SELECT strata...,
       ARRAY_AGG(STRUCT(bucket, n) ORDER BY bucket) AS node_person_hll
FROM m
GROUP BY strata...
```

**Merge** (union 2+ sketches)

```sql
SELECT bucket, MAX(n) AS n
FROM (
  SELECT bucket, n FROM UNNEST(sketchA)
  UNION ALL
  SELECT bucket, n FROM UNNEST(sketchB)
)
GROUP BY bucket
```

**Size** ~16 B × nonzero_buckets. INT64 each.

- **Pros**: human-readable in BQ console. Easy SQL merge via UNNEST.
- **Cons**: 2× overhead vs B (both fields INT64 — wasteful when bucket fits in 10 bits, n in 6).
- **Use when**: introspection / debugging matters, strata mostly small.

---

## B. Sparse packed int

**Schema**

```sql
node_person_hll ARRAY<INT64>
```

**Encoding**

Each element packs one `(bucket, n)` pair into a single INT64:

```
packed = bucket * 64 + n     -- bucket in high 10 bits, n in low 6 bits
bucket = packed >> 6
n      = packed & 63
```

64 chosen because `n ≤ 38 < 64 = 2^6`. Fits cleanly.

**Example row** same 4 buckets:

```
[2691, 8193, 46277, 57602]
```

Where `2691 = 42*64 + 3`, etc.

**Build** — same as A but `ARRAY_AGG(bucket*64 + n)`.

**Merge** — decode, MAX, re-encode:

```sql
SELECT (bucket << 6) | n_max AS packed FROM (
  SELECT bucket, MAX(n) AS n_max FROM (
    SELECT (p >> 6) AS bucket, (p & 63) AS n
    FROM UNNEST(sketchA) p
    UNION ALL
    SELECT (p >> 6), (p & 63) FROM UNNEST(sketchB) p
  )
  GROUP BY bucket
)
```

**Size** ~8 B × nonzero_buckets. Half of A.

- **Pros**: smallest sparse form, mergeable in SQL.
- **Cons**: opaque numbers in console. Encode/decode boilerplate.
- **Use when**: storage tight, most strata sparse, accepting decode at API layer.

---

## C. Dense int array

**Schema**

```sql
node_person_hll ARRAY<INT64>   -- length always 1024
```

**Encoding**

Index = bucket. Value = n. Bucket field disappears — implicit by array position.

```
hll[0]    = n for bucket 0
hll[1]    = n for bucket 1
...
hll[1023] = n for bucket 1023
```

Empty buckets = 0.

**Example** (mostly zeros):

```
[0, 0, ..., 3 at index 42, ..., 5 at index 723, ..., 0]
```

**Build** — produce dense with `GENERATE_ARRAY`:

```sql
WITH b AS (
  SELECT strata..., bucket, MAX(n) AS n FROM ... GROUP BY ...
)
SELECT strata...,
  ARRAY(
    SELECT COALESCE(MAX(IF(bucket = i, n, NULL)), 0)
    FROM b
    WHERE strata... = outer_strata...
    CROSS JOIN UNNEST(GENERATE_ARRAY(0, 1023)) AS i
    GROUP BY i ORDER BY i
  ) AS node_person_hll
FROM ...
```

(In practice cleaner with a join + array_agg pattern.)

**Merge** — element-wise MAX. BQ has no zip; you do it with index:

```sql
SELECT ARRAY(
  SELECT MAX(v) FROM UNNEST(
    [a[OFFSET(i)], b[OFFSET(i)]]
  ) v
  FROM UNNEST(GENERATE_ARRAY(0, 1023)) i
  ORDER BY i
)
```

Verbose. Often easier in a UDF.

**Size** 1024 × 8 B = **8 KB fixed**, regardless of distinct count.

- **Pros**: index = bucket (no encoding). Direct lookup. Element-wise ops natural.
- **Cons**: 8 KB always — wasteful for sparse. INT64 stores 6-bit value.
- **Use when**: every stratum is large (≫ 1024 distinct persons). Rarely a good fit.

---

## D. Dense bytes

**Schema**

```sql
node_person_hll BYTES   -- length always 1024
```

**Encoding**

One byte per bucket. Byte i = n for bucket i.

**Example** (16 hex chars per 8 bytes):

```
0x00 00 00 ... 03 ... 05 ... 00     -- 1024 bytes total
```

**Build** — concatenate bytes from per-bucket aggregated rows:

```sql
WITH b AS (
  SELECT strata..., bucket, MAX(n) AS n FROM ... GROUP BY ...
)
SELECT strata...,
  STRING_AGG(
    CASE WHEN n IS NULL THEN CAST(b'\x00' AS BYTES)
         ELSE BYTE(CAST(n AS INT64)) END,
    b'' ORDER BY i
  ) AS node_person_hll
FROM (
  SELECT strata..., i,
         (SELECT n FROM b WHERE b.bucket = i AND b.strata = outer.strata) AS n
  FROM outer_strata, UNNEST(GENERATE_ARRAY(0,1023)) i
) ...
```

BigQuery is awkward at building BYTES like this. Almost always wants a UDF.

**Merge** — needs JS UDF; native BYTES ops are limited (`BIT_OR`, `BIT_AND`, no element-wise MAX):

```js
CREATE TEMP FUNCTION merge_hll(a BYTES, b BYTES) RETURNS BYTES
LANGUAGE js AS r"""
  const A = new Uint8Array(a); const B = new Uint8Array(b);
  const out = new Uint8Array(1024);
  for (let i = 0; i < 1024; i++) out[i] = Math.max(A[i], B[i]);
  return out;
""";
```

**Size** 1024 B fixed.

- **Pros**: smallest dense form. 8× smaller than C.
- **Cons**: opaque. Needs UDF to build, merge, inspect. JS UDFs slower than native.
- **Use when**: scale demands minimum storage and you accept UDF cost.

---

## E. Native HLL

**Schema**

```sql
node_person_hll BYTES   -- HLL++ sketch in Google's internal format
```

**Build** — single function:

```sql
SELECT strata...,
       HLL_COUNT.INIT(CAST(person_id AS STRING), 10) AS node_person_hll
FROM events
GROUP BY strata...
```

**Merge**

```sql
SELECT strata..., HLL_COUNT.MERGE_PARTIAL(node_person_hll) AS merged
FROM t GROUP BY strata_partial...
```

Cardinality estimate:

```sql
SELECT HLL_COUNT.EXTRACT(merged) FROM ...
```

**Size** Google's HLL++ has sparse + dense modes; ~few hundred B (small) to
~1.5 KB (saturated dense). Auto.

- **Pros**: fastest. Native, optimised. No custom code. Sparse↔dense auto.
- **Cons**: completely opaque — cannot expose `(bucket, n)` pairs. Format is
  BQ-specific; SQLite cannot merge it. Defeats your stated API contract.
- **Use when**: BQ-only, no need to inspect, just want cardinality numbers.

---

## Summary

| option | introspect | smallest small | smallest big | merge ease | cross-DB |
|---|---|---|---|---|---|
| A | best | bad (2× B) | bad | easy SQL | possible |
| B | partial | best | mid | easy SQL | possible |
| C | yes | terrible | mid | verbose | possible |
| D | UDF only | terrible | best | UDF | possible |
| E | no | auto | best | native | BQ-only |

For an API contract that returns a list of `(bucket, n)`: **A or B** at storage,
decode at the API. B if storage cost matters; A if it does not.
