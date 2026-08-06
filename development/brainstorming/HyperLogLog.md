# HyperLogLog Summary (p = 10)

## Goal

Represent a set as a small fixed-size sketch that can later be merged with other sketches to estimate the number of distinct elements in the union.

---

## Parameters

* Hash size: **48 bits**
* Precision: **p = 10**
* Number of buckets:

[
m = 2^{10} = 1024
]

Each 48-bit hash is split into:

```text
+----------+----------------------------------+
| 10 bits  |            38 bits               |
+----------+----------------------------------+
| Bucket   | Leading-zero measurement region  |
+----------+----------------------------------+
```

---

## Encoding a Set

Suppose we have:

[
A = {apple, banana, cherry}
]

Hash each element to a 48-bit value.

### Example Hashes

| Element | 48-bit Hash (simplified)   |
| ------- | -------------------------- |
| apple   | 1011010011 000100000000... |
| banana  | 0110101010 001000000000... |
| cherry  | 1011010011 000001000000... |

---

### Step 1: Determine the Bucket

Take the first 10 bits.

For example:

```text
apple
1011010011 | 000100000000...
^^^^^^^^^^
bucket 723
```

```text
banana
0110101010 | 001000000000...
^^^^^^^^^^
bucket 426
```

```text
cherry
1011010011 | 000001000000...
^^^^^^^^^^
bucket 723
```

---

### Step 2: Count Leading Zeros

Count the leading zeros in the remaining 38 bits.

Example:

```text
apple suffix

000100000000...

leading zeros = 3
```

```text
cherry suffix

000001000000...

leading zeros = 5
```

---

### Step 3: Update Registers

Each bucket stores the maximum value seen.

For bucket 723:

```text
apple  -> 3
cherry -> 5

register[723] = max(3,5) = 5
```

For bucket 426:

```text
banana -> 2

register[426] = 2
```

The sketch for Set A now contains:

| Bucket | Value |
| ------ | ----- |
| 426    | 2     |
| 723    | 5     |

All other buckets remain 0.

---

## Encoding Another Set

Suppose:

[
B = {banana, date, fig}
]

After hashing and processing:

| Bucket | Value |
| ------ | ----- |
| 426    | 2     |
| 512    | 4     |
| 723    | 3     |

This is the sketch for Set B.

---

## Union of Two Compressed Sets

To compute:

[
A \cup B
]

we do **not** need the original elements.

Instead, merge the sketches bucket-by-bucket:

[
R_{union}[i]
============

\max(R_A[i], R_B[i])
]

### Example

| Bucket | A | B | Union |
| ------ | - | - | ----- |
| 426    | 2 | 2 | 2     |
| 512    | 0 | 4 | 4     |
| 723    | 5 | 3 | 5     |

Resulting sketch:

| Bucket | Value |
| ------ | ----- |
| 426    | 2     |
| 512    | 4     |
| 723    | 5     |

This sketch now represents:

[
A \cup B
]

without storing any of the original elements.

---

## Cardinality Estimation

HyperLogLog estimates the number of distinct elements from the merged registers.

For (m = 1024):

[
E
=

\alpha_m
\cdot m^2
\cdot
\left(
\sum_{i=1}^{m}
2^{-R_i}
\right)^{-1}
]

where:

* (R_i) = value stored in bucket (i)
* (m = 1024)
* (\alpha_m \approx 0.7205)

The result is an estimate of:

[
|A \cup B|
]

---

## Key Property

Given only the compressed sketches:

[
H(A)
]

and

[
H(B)
]

we can estimate:

[
|A \cup B|
]

using:

[
H(A \cup B)
===========

\max(H(A), H(B))
]

(register-wise maximum)

and then:

[
|A \cup B|
\approx
Estimate(H(A \cup B))
]

No original elements are required.

---

## Characteristics (p = 10)

| Property                       | Value      |
| ------------------------------ | ---------- |
| Buckets                        | 1,024      |
| Hash bits for bucket selection | 10         |
| Hash bits for zero counting    | 38         |
| Relative error                 | ~3.25%     |
| Memory usage                   | ~768 bytes |
| Mergeable                      | Yes        |
| Recover original elements      | No         |
| Supports union                 | Yes        |
