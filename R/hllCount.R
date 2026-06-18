# Aggregate (merge) HyperLogLog++ sketches in pure R.
#
# Port of BigQuery `HLL_COUNT.MERGE_PARTIAL` for sketches built by
# `HLL_COUNT.INIT` (ZetaSketch HLL++ encoding_version 2). Drop-in for
# `sum()` inside `dplyr::summarise()` on a column of base64 (or raw) blobs.
#
# Reference impl: dev/brainstorming/hll/mergeHll.js
#  - Same dual-path merge (sparse+sparse lossless union, otherwise dense pmax).
#  - Same sparse->dense layout (idxPrime >> 1; mid bits decode rho).
#  - No HLL bias-correction table (use BQ HLL_COUNT.EXTRACT for parity).

.HLL_TYPE <- 112L
.ENCODING_VERSION <- 2L

# ---- varint codec --------------------------------------------------------

.readVarint <- function(bytes, off) {
  v <- 0
  shift <- 0L
  repeat {
    b <- as.integer(bytes[off])
    off <- off + 1L
    v <- v + bitwAnd(b, 0x7fL) * (2 ^ shift)
    if (bitwAnd(b, 0x80L) == 0L) break
    shift <- shift + 7L
    if (shift > 56L) stop("varint exceeds supported width")
  }
  list(value = v, off = off)
}

.writeVarint <- function(v) {
  out <- integer(10L)
  n <- 0L
  while (v >= 128) {
    n <- n + 1L
    out[n] <- bitwOr(as.integer(v %% 128), 0x80L)
    v <- floor(v / 128)
  }
  n <- n + 1L
  out[n] <- as.integer(v)
  as.raw(out[seq_len(n)])
}

# ---- proto field writers -------------------------------------------------

.writeTag <- function(field, wireType) {
  .writeVarint(bitwOr(bitwShiftL(field, 3L), wireType))
}

.writeVarintField <- function(field, v) {
  c(.writeTag(field, 0L), .writeVarint(v))
}

.writeBytesField <- function(field, data) {
  c(.writeTag(field, 2L), .writeVarint(length(data)), data)
}

# ---- proto parsers -------------------------------------------------------

.parseAggregator <- function(bytes) {
  out <- list(
    type = NA_integer_, numValues = 0, encodingVersion = NA_integer_,
    valueType = NA_integer_, hllExt = raw(0)
  )
  off <- 1L
  n <- length(bytes)
  while (off <= n) {
    r <- .readVarint(bytes, off); tag <- as.integer(r$value); off <- r$off
    field <- bitwShiftR(tag, 3L)
    wireType <- bitwAnd(tag, 7L)
    if (wireType == 0L) {
      r <- .readVarint(bytes, off); val <- r$value; off <- r$off
      if (field == 1L) out$type <- as.integer(val)
      else if (field == 2L) out$numValues <- val
      else if (field == 3L) out$encodingVersion <- as.integer(val)
      else if (field == 4L) out$valueType <- as.integer(val)
    } else if (wireType == 2L) {
      r <- .readVarint(bytes, off); len <- as.integer(r$value); off <- r$off
      payload <- if (len > 0L) bytes[off:(off + len - 1L)] else raw(0)
      off <- off + len
      if (field == 112L) out$hllExt <- payload
    } else {
      stop(sprintf("unsupported wire type %d for field %d", wireType, field))
    }
  }
  out
}

.parseInner <- function(bytes) {
  out <- list(
    sparseSize = NA_integer_, precision = NA_integer_,
    sparsePrecision = NA_integer_, data = raw(0), sparseData = raw(0)
  )
  off <- 1L
  n <- length(bytes)
  while (off <= n) {
    r <- .readVarint(bytes, off); tag <- as.integer(r$value); off <- r$off
    field <- bitwShiftR(tag, 3L)
    wireType <- bitwAnd(tag, 7L)
    if (wireType == 0L) {
      r <- .readVarint(bytes, off); val <- r$value; off <- r$off
      if (field == 2L) out$sparseSize <- as.integer(val)
      else if (field == 3L) out$precision <- as.integer(val)
      else if (field == 4L) out$sparsePrecision <- as.integer(val)
    } else if (wireType == 2L) {
      r <- .readVarint(bytes, off); len <- as.integer(r$value); off <- r$off
      payload <- if (len > 0L) bytes[off:(off + len - 1L)] else raw(0)
      off <- off + len
      if (field == 5L) out$data <- payload
      else if (field == 6L) out$sparseData <- payload
    } else {
      stop(sprintf("unsupported wire type %d for field %d", wireType, field))
    }
  }
  out
}

# ---- sparse codec --------------------------------------------------------

.decodeSparseEntries <- function(bytes) {
  n <- length(bytes)
  if (n == 0L) return(integer(0))
  out <- integer(n)
  off <- 1L
  prev <- 0
  count <- 0L
  while (off <= n) {
    r <- .readVarint(bytes, off); off <- r$off
    prev <- prev + r$value
    count <- count + 1L
    out[count] <- as.integer(prev)
  }
  out[seq_len(count)]
}

.encodeSparseEntries <- function(sortedPacked) {
  if (length(sortedPacked) == 0L) return(raw(0))
  parts <- vector("list", length(sortedPacked))
  prev <- 0
  for (i in seq_along(sortedPacked)) {
    parts[[i]] <- .writeVarint(sortedPacked[i] - prev)
    prev <- sortedPacked[i]
  }
  do.call(c, parts)
}

# ---- sparse -> dense -----------------------------------------------------

.sparseEntryToDense <- function(packed, p, pPrime) {
  idxPrime <- bitwShiftR(packed, 1L)
  midWidth <- pPrime - p
  midMask <- bitwShiftL(1L, midWidth) - 1L
  denseIdx <- bitwShiftR(idxPrime, midWidth)
  mid <- bitwAnd(idxPrime, midMask)
  if (mid != 0L) {
    bitLen <- 0L
    m <- mid
    while (m > 0L) { bitLen <- bitLen + 1L; m <- bitwShiftR(m, 1L) }
    denseRho <- midWidth - bitLen + 1L
  } else {
    denseRho <- midWidth + 1L
  }
  c(denseIdx, denseRho)
}

.toDense <- function(inner) {
  p <- inner$precision
  m <- bitwShiftL(1L, p)
  regs <- integer(m)
  if (length(inner$data) > 0L) {
    if (length(inner$data) != m) {
      stop(sprintf("dense data length %d != 2^p (%d)", length(inner$data), m))
    }
    regs <- as.integer(inner$data)
  }
  if (length(inner$sparseData) > 0L) {
    pPrime <- inner$sparsePrecision
    if (is.na(pPrime) || pPrime < p) stop("invalid sparse_precision")
    entries <- .decodeSparseEntries(inner$sparseData)
    if (!is.na(inner$sparseSize) && length(entries) != inner$sparseSize) {
      stop(sprintf("sparse_size %d != decoded %d",
                   inner$sparseSize, length(entries)))
    }
    for (e in entries) {
      ir <- .sparseEntryToDense(e, p, pPrime)
      i <- ir[1L] + 1L
      if (ir[2L] > regs[i]) regs[i] <- ir[2L]
    }
  }
  list(p = p, regs = regs)
}

# ---- merge ---------------------------------------------------------------

.mergePair <- function(bytesA, bytesB) {
  aggA <- .parseAggregator(bytesA)
  aggB <- .parseAggregator(bytesB)
  if (isTRUE(aggA$type != .HLL_TYPE)) stop(sprintf("type %d != %d", aggA$type, .HLL_TYPE))
  if (isTRUE(aggB$type != .HLL_TYPE)) stop(sprintf("type %d != %d", aggB$type, .HLL_TYPE))
  if (isTRUE(aggA$encodingVersion != .ENCODING_VERSION)) {
    stop(sprintf("encoding_version %d unsupported (need %d)",
                 aggA$encodingVersion, .ENCODING_VERSION))
  }
  if (isTRUE(aggA$valueType != aggB$valueType)) {
    stop(sprintf("value_type mismatch %d vs %d", aggA$valueType, aggB$valueType))
  }
  innerA <- .parseInner(aggA$hllExt)
  innerB <- .parseInner(aggB$hllExt)
  if (isTRUE(innerA$precision != innerB$precision)) {
    stop(sprintf("precision mismatch %d vs %d", innerA$precision, innerB$precision))
  }

  innerSparseP <- if (!is.na(innerA$sparsePrecision)) innerA$sparsePrecision
                  else if (!is.na(innerB$sparsePrecision)) innerB$sparsePrecision
                  else innerA$precision

  bothSparse <- length(innerA$sparseData) > 0L && length(innerB$sparseData) > 0L &&
                length(innerA$data) == 0L && length(innerB$data) == 0L

  innerOut <- if (bothSparse &&
                  isTRUE(innerA$sparsePrecision == innerB$sparsePrecision)) {
    entA <- .decodeSparseEntries(innerA$sparseData)
    entB <- .decodeSparseEntries(innerB$sparseData)
    sorted <- sort(unique(c(entA, entB)))
    sparseData <- .encodeSparseEntries(sorted)
    c(
      .writeVarintField(2L, length(sorted)),
      .writeVarintField(3L, innerA$precision),
      .writeVarintField(4L, innerSparseP),
      .writeBytesField(6L, sparseData)
    )
  } else {
    da <- .toDense(innerA)
    db <- .toDense(innerB)
    merged <- pmax(da$regs, db$regs)
    c(
      .writeVarintField(3L, da$p),
      .writeVarintField(4L, innerSparseP),
      .writeBytesField(5L, as.raw(merged))
    )
  }

  numValuesSum <- (aggA$numValues %||% 0) + (aggB$numValues %||% 0)

  c(
    .writeVarintField(1L, .HLL_TYPE),
    .writeVarintField(2L, numValuesSum),
    .writeVarintField(3L, .ENCODING_VERSION),
    .writeVarintField(4L, aggA$valueType %||% 0L),
    .writeBytesField(112L, innerOut)
  )
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

#' HLL_COUNT.MERGE_PARTIAL — aggregate HyperLogLog++ sketches.
#'
#' Pure-R port of BigQuery `HLL_COUNT.MERGE_PARTIAL`. Drop-in for `sum()`
#' inside `dplyr::summarise()` on a column of HLL sketches produced by
#' `HLL_COUNT.INIT` (precision 10–18, encoding_version 2). Returns the
#' merged sketch (not a cardinality — see `HLL_COUNT.EXTRACT` for that).
#'
#' @param sketches base64 character vector, OR list of raw vectors, OR a
#'   single raw vector. NA / NULL / empty entries are dropped.
#' @return If input was character: base64 string of the merged sketch.
#'   If input was raw / list of raws: raw vector. `NA` if no usable sketches
#'   remain after filtering.
#' @export
HLL_COUNT.MERGE_PARTIAL <- function(sketches) {
  if (is.raw(sketches)) sketches <- list(sketches)
  if (length(sketches) == 0L) return(NA)

  asChar <- is.character(sketches)
  if (asChar) {
    keep <- !is.na(sketches) & nzchar(sketches)
    sketches <- sketches[keep]
    if (length(sketches) == 0L) return(NA)
    if (length(sketches) == 1L) return(sketches[[1L]])
    rawList <- lapply(sketches, base64enc::base64decode)
  } else if (is.list(sketches)) {
    sketches <- Filter(function(x) !is.null(x) && length(x) > 0L, sketches)
    if (length(sketches) == 0L) return(NA)
    if (length(sketches) == 1L) return(sketches[[1L]])
    rawList <- sketches
  } else {
    stop("sketches must be character, raw, or list of raws")
  }

  merged <- Reduce(.mergePair, rawList)
  if (asChar) base64enc::base64encode(merged) else merged
}
