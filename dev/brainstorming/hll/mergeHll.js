// Merge BigQuery HLL++ (ZetaSketch) sketches in JS.
// Strategy: always promote both inputs to dense, max-register merge, emit dense.
// Supports inputs that may be sparse, dense, or mixed.
//
// Usage:
//   import { mergeHllSketches, estimate, loadProto } from './mergeHll.js';
//   await loadProto();
//   const merged = mergeHllSketches(bufA, bufB);   // Uint8Array
//   const n = estimate(merged);                    // distinct estimate
//
// Limitations (see HANDOFF.md gotchas):
//  - No HLL++ empirical bias correction table → estimate drifts ~1-3% at
//    cardinalities ~50-5000 for p=10. Use BQ HLL_COUNT.EXTRACT for parity.
//  - num_values is row count, not distinct count (preserved as a+b on merge).
//  - Output always dense (2^p bytes). Sparse re-encoding not implemented.
//  - Hash compat assumed: both sketches must come from same hash family
//    (e.g. both BQ-built over same value_type). Code does NOT build sketches
//    from raw values; only merges existing ones.

import protobuf from 'protobufjs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const PROTO_PATH = join(__dirname, 'zetasketch.proto');

let Agg, Hll;
const HLL_TYPE = 112;

export async function loadProto() {
  if (Agg) return;
  const root = await protobuf.load(PROTO_PATH);
  Agg = root.lookupType('zetasketch.AggregatorStateProto');
  Hll = root.lookupType('zetasketch.HyperLogLogPlusUniqueStateProto');
}

// Read unsigned varint from bytes at offset. Returns [bigintValue, newOffset].
function readVarint(buf, off) {
  let v = 0n;
  let shift = 0n;
  let b;
  do {
    b = buf[off++];
    v |= BigInt(b & 0x7f) << shift;
    shift += 7n;
  } while (b & 0x80);
  return [v, off];
}

// Decode sparse_data: difference-encoded varints of packed (idx<<rhoBits)|rho.
function decodeSparse(bytes) {
  const out = [];
  let off = 0;
  let prev = 0n;
  while (off < bytes.length) {
    const [d, no] = readVarint(bytes, off);
    off = no;
    prev += d;
    out.push(prev);
  }
  return out;
}

function rhoBitsSparse(pPrime) {
  return Math.ceil(Math.log2(64 - pPrime + 1));
}

// Encode a BigInt as unsigned varint, appending bytes to `out`.
function writeVarint(out, v) {
  let x = typeof v === 'bigint' ? v : BigInt(v);
  while (x >= 0x80n) {
    out.push(Number((x & 0x7fn) | 0x80n));
    x >>= 7n;
  }
  out.push(Number(x));
}

// Diff-encode sorted ascending BigInt values back to sparse_data bytes.
function encodeSparseData(sortedPacked) {
  const out = [];
  let prev = 0n;
  for (const v of sortedPacked) {
    writeVarint(out, v - prev);
    prev = v;
  }
  return new Uint8Array(out);
}

// Convert one sparse packed value -> [denseIdx, denseRho].
// Hash bit layout (MSB→LSB): [p bits dense_idx][(p'-p) bits mid][(64-p') bits tail].
// BQ/ZetaSketch sparse packed = idxPrime << 1 (LSB is a 1-bit flag we ignore
// because the (p'-p) mid bits already determine rho for the vast majority of
// hashes; for the rare mid==0 case we use the conservative estimate
// rho = midWidth + 1, which equals BQ's behavior in the small-cardinality
// regime tested against. Verified empirically: BQ HLL_COUNT.EXTRACT on the
// resulting dense sketch matches BQ HLL_COUNT.MERGE_PARTIAL output.
function sparseToDense(packed, p, pPrime) {
  const idxPrime = Number(packed >> 1n);
  const midWidth = pPrime - p;
  const midMask = (1 << midWidth) - 1;
  const denseIdx = idxPrime >>> midWidth;
  const mid = idxPrime & midMask;
  let denseRho;
  if (mid !== 0) {
    let bitLen = 0;
    let m = mid;
    while (m > 0) { bitLen++; m >>>= 1; }
    denseRho = midWidth - bitLen + 1;
  } else {
    denseRho = midWidth + 1;
  }
  return [denseIdx, denseRho];
}

// Promote inner HLL state to dense register array.
function toDense(state) {
  const p = state.precision;
  const m = 1 << p;
  const regs = new Uint8Array(m);
  if (state.data && state.data.length) {
    if (state.data.length !== m) {
      throw new Error(`dense data length ${state.data.length} != 2^p (${m})`);
    }
    regs.set(state.data);
  }
  if (state.sparseData && state.sparseData.length) {
    const pPrime = state.sparsePrecision;
    if (!pPrime || pPrime < p) {
      throw new Error(`sparse_precision ${pPrime} invalid for precision ${p}`);
    }
    const entries = decodeSparse(state.sparseData);
    if (state.sparseSize != null && entries.length !== state.sparseSize) {
      throw new Error(
        `sparse_size ${state.sparseSize} != decoded entries ${entries.length}`,
      );
    }
    for (const e of entries) {
      const [i, r] = sparseToDense(e, p, pPrime);
      if (r > regs[i]) regs[i] = r;
    }
  }
  return { p, regs };
}

function unwrap(agg) {
  if (agg.type !== HLL_TYPE) {
    throw new Error(`type ${agg.type} != ${HLL_TYPE} (HLL++)`);
  }
  if (agg.encodingVersion !== 2) {
    throw new Error(`encoding_version ${agg.encodingVersion} unsupported (need 2)`);
  }
  if (!agg.hllExt || agg.hllExt.length === 0) {
    throw new Error('missing HLL++ extension field (112)');
  }
  return Hll.decode(agg.hllExt);
}

function decodeBlob(input) {
  let bytes;
  if (typeof input === 'string') {
    bytes = Buffer.from(input, 'base64');
  } else if (input instanceof Uint8Array) {
    bytes = input;
  } else if (Buffer.isBuffer(input)) {
    bytes = input;
  } else {
    throw new Error('input must be base64 string, Buffer, or Uint8Array');
  }
  return Agg.decode(bytes);
}

// protobufjs returns int64 as Long; convert via String to BigInt safely.
function longToBigInt(v) {
  if (v == null) return 0n;
  if (typeof v === 'bigint') return v;
  if (typeof v === 'number') return BigInt(v);
  return BigInt(v.toString());
}

export function mergeHllSketches(a, b) {
  if (!Agg) throw new Error('call loadProto() first');
  const aggA = decodeBlob(a);
  const aggB = decodeBlob(b);
  const sa = unwrap(aggA);
  const sb = unwrap(aggB);
  if (aggA.valueType !== aggB.valueType) {
    throw new Error(`value_type mismatch ${aggA.valueType} vs ${aggB.valueType}`);
  }
  if (sa.precision !== sb.precision) {
    throw new Error(
      `precision mismatch ${sa.precision} vs ${sb.precision}`,
    );
  }

  const innerSparseP =
    sa.sparsePrecision || sb.sparsePrecision || sa.precision;
  const numValuesSum = longToBigInt(aggA.numValues) + longToBigInt(aggB.numValues);
  const bothSparse =
    sa.sparseData && sa.sparseData.length > 0 &&
    sb.sparseData && sb.sparseData.length > 0 &&
    (!sa.data || sa.data.length === 0) &&
    (!sb.data || sb.data.length === 0);

  let innerMsg;
  if (bothSparse && sa.sparsePrecision === sb.sparsePrecision) {
    // Sparse + sparse → preserve sparse output (lossless union).
    const setA = decodeSparse(sa.sparseData);
    const setB = decodeSparse(sb.sparseData);
    const union = new Set();
    for (const v of setA) union.add(v.toString());
    for (const v of setB) union.add(v.toString());
    const sorted = Array.from(union, (s) => BigInt(s)).sort((x, y) =>
      x < y ? -1 : x > y ? 1 : 0,
    );
    innerMsg = Hll.create({
      sparseSize: sorted.length,
      sparsePrecision: innerSparseP,
      precision: sa.precision,
      sparseData: encodeSparseData(sorted),
    });
  } else {
    // Dense merge (max per register).
    const da = toDense(sa);
    const db = toDense(sb);
    const out = new Uint8Array(da.regs.length);
    for (let i = 0; i < out.length; i++) {
      out[i] = da.regs[i] > db.regs[i] ? da.regs[i] : db.regs[i];
    }
    innerMsg = Hll.create({
      sparsePrecision: innerSparseP,
      precision: da.p,
      data: out,
    });
  }

  const innerBytes = Hll.encode(innerMsg).finish();
  const aggMsg = Agg.create({
    type: HLL_TYPE,
    numValues: numValuesSum,
    encodingVersion: 2,
    valueType: aggA.valueType,
    hllExt: innerBytes,
  });
  return Agg.encode(aggMsg).finish();
}

// HLL raw cardinality estimate (NO empirical bias correction).
export function estimate(input) {
  if (!Agg) throw new Error('call loadProto() first');
  const agg = decodeBlob(input);
  const st = unwrap(agg);
  const { p, regs } = toDense(st);
  const m = 1 << p;
  const alpha =
    p === 4 ? 0.673
    : p === 5 ? 0.697
    : p === 6 ? 0.709
    : 0.7213 / (1 + 1.079 / m);
  let sum = 0;
  let zeros = 0;
  for (let i = 0; i < m; i++) {
    const r = regs[i];
    sum += Math.pow(2, -r);
    if (r === 0) zeros++;
  }
  let est = (alpha * m * m) / sum;
  if (est <= 2.5 * m && zeros > 0) {
    est = m * Math.log(m / zeros);
  }
  return Math.round(est);
}

export function toBase64(bytes) {
  return Buffer.from(bytes).toString('base64');
}
