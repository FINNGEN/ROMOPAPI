// Test JS HLL merge against BigQuery HLL_COUNT.MERGE_PARTIAL.
//
// Strategy:
//   1. Build two sketches in BQ from disjoint + overlapping integer sets at p=10.
//   2. Fetch both sketches (BYTES → base64) and the BQ-merged sketch.
//   3. Run JS merge on the two inputs.
//   4. Send JS-merged sketch BACK to BQ, call HLL_COUNT.EXTRACT on it and
//      on the BQ-merged sketch. Compare cardinalities.
//   5. Also compare JS estimate() to BQ extract (informational; bias-correction
//      gap means non-strict).
//
// Requires:
//   npm i @google-cloud/bigquery protobufjs
//   gcloud auth application-default login   (or GOOGLE_APPLICATION_CREDENTIALS)
//   env: BQ_PROJECT=<project-id>   (or pass --project)
//
// Run:
//   node testAgainstBq.js
//
// Exit code 0 = pass, 1 = fail.

import { BigQuery } from '@google-cloud/bigquery';
import {
  loadProto,
  mergeHllSketches,
  estimate,
  toBase64,
} from './mergeHll.js';

const PROJECT = process.env.BQ_PROJECT || process.argv[2];
if (!PROJECT) {
  console.error('set BQ_PROJECT env or pass project id as first arg');
  process.exit(2);
}
const PRECISION = 10;

const bq = new BigQuery({ projectId: PROJECT });

async function query(sql, params = {}, types = {}) {
  const [job] = await bq.createQueryJob({
    query: sql,
    params,
    types,
    parameterMode: 'named',
  });
  const [rows] = await job.getQueryResults();
  return rows;
}

// NOTE: @google-cloud/bigquery mangles BYTES params (double-base64).
// Workaround: pass as STRING (base64), decode via FROM_BASE64() in SQL.
function toB64(x) {
  return Buffer.from(x).toString('base64');
}

// Build a sketch over integers [lo, hi) at given precision; return Buffer.
async function buildSketch(lo, hi) {
  const sql = `
    SELECT HLL_COUNT.INIT(x, ${PRECISION}) AS sketch
    FROM UNNEST(GENERATE_ARRAY(@lo, @hi - 1)) AS x
  `;
  const rows = await query(sql, { lo, hi }, { lo: 'INT64', hi: 'INT64' });
  return rows[0].sketch; // Buffer
}

// Merge two sketches in BQ; return Buffer of merged sketch.
async function bqMerge(a, b) {
  const sql = `
    SELECT HLL_COUNT.MERGE_PARTIAL(s) AS merged
    FROM UNNEST([FROM_BASE64(@a), FROM_BASE64(@b)]) AS s
  `;
  const rows = await query(
    sql,
    { a: toB64(a), b: toB64(b) },
    { a: 'STRING', b: 'STRING' },
  );
  return rows[0].merged;
}

// Extract cardinality from a sketch via BQ.
async function bqExtract(sketch) {
  const sql = 'SELECT HLL_COUNT.EXTRACT(FROM_BASE64(@s)) AS n';
  const rows = await query(sql, { s: toB64(sketch) }, { s: 'STRING' });
  return Number(rows[0].n);
}

function assertEq(label, got, want) {
  const ok = got === want;
  const tag = ok ? 'PASS' : 'FAIL';
  console.log(`  [${tag}] ${label}: got=${got} want=${want}`);
  return ok;
}

async function runCase(label, a, b, expectedDistinct) {
  console.log(`\n--- ${label} ---`);
  const sketchA = await buildSketch(...a);
  const sketchB = await buildSketch(...b);
  const bqMerged = await bqMerge(sketchA, sketchB);
  const bqMergedCard = await bqExtract(bqMerged);

  const jsMerged = mergeHllSketches(sketchA, sketchB);
  const jsMergedCard = await bqExtract(Buffer.from(jsMerged));
  const jsLocalEst = estimate(jsMerged);

  console.log(`  bq_merged_card     = ${bqMergedCard}`);
  console.log(`  js_merged_card(bq) = ${jsMergedCard}`);
  console.log(`  js_local_estimate  = ${jsLocalEst}`);
  console.log(`  true_distinct      = ${expectedDistinct}`);

  let ok = true;
  // Strict: BQ extract on JS-merged sketch must equal BQ extract on
  // BQ-merged sketch. Both run BQ's bias correction, so values match exactly.
  ok = assertEq('js-merged sketch matches bq-merged sketch', jsMergedCard, bqMergedCard) && ok;
  // Informational: local estimate within reasonable error.
  const relErr = Math.abs(jsLocalEst - expectedDistinct) / expectedDistinct;
  console.log(`  local_rel_error    = ${(relErr * 100).toFixed(2)}%`);
  return ok;
}

async function main() {
  await loadProto();
  let allOk = true;

  // Case 1: disjoint, small (sparse + sparse).
  allOk = (await runCase('disjoint small', [0, 5], [100, 105], 10)) && allOk;

  // Case 2: overlapping, small.
  allOk = (await runCase('overlap small', [0, 50], [25, 75], 75)) && allOk;

  // Case 3: medium, mostly disjoint (likely sparse → dense crossover).
  allOk = (await runCase('medium disjoint', [0, 2000], [10000, 12000], 4000)) && allOk;

  // Case 4: large, disjoint (both dense).
  allOk = (await runCase('large disjoint', [0, 50000], [100000, 150000], 100000)) && allOk;

  // Case 5: identical sets.
  allOk = (await runCase('identical', [0, 1000], [0, 1000], 1000)) && allOk;

  console.log(`\n${allOk ? 'ALL PASS' : 'SOME FAILED'}`);
  process.exit(allOk ? 0 : 1);
}

main().catch((e) => {
  console.error('error:', e);
  process.exit(1);
});
