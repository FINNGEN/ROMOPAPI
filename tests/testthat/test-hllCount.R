# Tests for sumHLL — pure-R HLL++ merge.
#
# Mirrors dev/brainstorming/hll/testAgainstBq.js semantically: same input
# sketches built by `HLL_COUNT.INIT(x, 10)` over known integer sets. Where
# the JS test calls BQ to extract cardinality from each merge, this test
# decodes the merged sketch directly and verifies the sparse entries are
# the lossless union of the inputs (which is what `HLL_COUNT.MERGE_PARTIAL`
# guarantees in the sparse+sparse regime).
#
# Input fixtures (base64) are real BQ outputs harvested via the JS test
# harness in `dev/brainstorming/hll/`.

# BQ HLL_COUNT.INIT(GENERATE_ARRAY(1,5), 10)  — 5 distinct, sparse.
.sketch_A <- "CHAQBRgCIAiCBxIQBRgKIA8yCt8fkmTTHOwU3AM="

# BQ HLL_COUNT.INIT(GENERATE_ARRAY(6,8), 10)  — 3 distinct, sparse.
.sketch_B <- "CHAQAxgCIAiCBw8QAxgKIA8yB5kVvg7buAE="

.decode_inner <- function(b64) {
  bytes <- base64enc::base64decode(b64)
  agg <- ROMOPAPI:::.parseAggregator(bytes)
  ROMOPAPI:::.parseInner(agg$hllExt)
}

test_that("sumHLL: sparse+sparse merge produces lossless union of entries", {
  merged <- sumHLL(c(.sketch_A, .sketch_B))
  expect_type(merged, "character")

  innerA <- .decode_inner(.sketch_A)
  innerB <- .decode_inner(.sketch_B)
  innerM <- .decode_inner(merged)

  entA <- ROMOPAPI:::.decodeSparseEntries(innerA$sparseData)
  entB <- ROMOPAPI:::.decodeSparseEntries(innerB$sparseData)
  expected <- sort(unique(c(entA, entB)))

  entM <- ROMOPAPI:::.decodeSparseEntries(innerM$sparseData)
  expect_equal(entM, expected)
  expect_equal(innerM$sparseSize, length(expected))
  expect_equal(innerM$precision, innerA$precision)
  expect_equal(innerM$sparsePrecision, innerA$sparsePrecision)
})

test_that("sumHLL: outer aggregator metadata preserved", {
  merged <- sumHLL(c(.sketch_A, .sketch_B))
  bytes <- base64enc::base64decode(merged)
  agg <- ROMOPAPI:::.parseAggregator(bytes)
  expect_equal(agg$type, 112L)
  expect_equal(agg$encodingVersion, 2L)
  # value_type 8 = BQ INT64 (HLL_COUNT.INIT on integer column).
  expect_equal(agg$valueType, 8L)
  # num_values summed across inputs (5 + 3).
  expect_equal(agg$numValues, 8)
})

test_that("sumHLL: singleton passthrough (character)", {
  expect_identical(sumHLL(.sketch_A), .sketch_A)
  expect_identical(sumHLL(c(.sketch_A)), .sketch_A)
})

test_that("sumHLL: singleton passthrough (raw)", {
  raw_a <- base64enc::base64decode(.sketch_A)
  expect_identical(sumHLL(raw_a), raw_a)
  expect_identical(sumHLL(list(raw_a)), raw_a)
})

test_that("sumHLL: empty / NA returns NA", {
  expect_true(is.na(sumHLL(character(0))))
  expect_true(is.na(sumHLL(NA_character_)))
  expect_true(is.na(sumHLL(c(NA_character_, NA_character_))))
  expect_true(is.na(sumHLL(list())))
})

test_that("sumHLL: NA entries dropped, remaining merged", {
  merged <- sumHLL(c(NA_character_, .sketch_A, NA_character_, .sketch_B))
  expect_type(merged, "character")
  innerM <- .decode_inner(merged)
  expect_equal(innerM$sparseSize, 8L)
})

test_that("sumHLL: raw and character routes agree byte-for-byte", {
  ar <- base64enc::base64decode(.sketch_A)
  br <- base64enc::base64decode(.sketch_B)
  merged_raw <- sumHLL(list(ar, br))
  merged_b64 <- sumHLL(c(.sketch_A, .sketch_B))
  expect_true(is.raw(merged_raw))
  expect_equal(base64enc::base64encode(merged_raw), merged_b64)
})

test_that("sumHLL: merging a sketch with itself is idempotent (cardinality-wise)", {
  doubled <- sumHLL(c(.sketch_A, .sketch_A))
  innerA <- .decode_inner(.sketch_A)
  innerD <- .decode_inner(doubled)
  entA <- ROMOPAPI:::.decodeSparseEntries(innerA$sparseData)
  entD <- ROMOPAPI:::.decodeSparseEntries(innerD$sparseData)
  expect_equal(entD, sort(unique(entA)))
})

test_that("sumHLL: N-way reduce equals pairwise reduce", {
  m_ab <- sumHLL(c(.sketch_A, .sketch_B))
  m_all <- sumHLL(c(.sketch_A, .sketch_B, .sketch_A))
  inner_ab <- .decode_inner(m_ab)
  inner_all <- .decode_inner(m_all)
  ent_ab <- ROMOPAPI:::.decodeSparseEntries(inner_ab$sparseData)
  ent_all <- ROMOPAPI:::.decodeSparseEntries(inner_all$sparseData)
  # Adding A again is a no-op on the entry set.
  expect_equal(ent_all, ent_ab)
})
