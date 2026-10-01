#!/usr/bin/env bash
# See usage() below (run with --help) for what this script does.

set -euo pipefail

usage() {
  cat <<EOF
Usage: $0 [--dry-run] <project.dataset> <gcs_output_path>

Export the three ROMOPAPI precomputed counts tables (stratified_code_counts,
stratified_persons, code_counts) from BigQuery straight to Google Cloud
Storage: one folder per table under gcs_output_path, each containing the
table data as CSV shard(s) (empty string for NULL cells) and the table
schema as schema.json. Uses \`bq extract\` (a server-side BigQuery job)
rather than \`bq query\`, so it stays fast even on large production tables
that would otherwise have to be paged through the CLI row by row.

Arguments:
  project.dataset    BigQuery project and dataset holding all 3 tables,
                      e.g. fg-production-sandbox-46.finngen_omop_result_dev
  gcs_output_path     Destination GCS path, e.g. gs://mytmpbucket/tmp

Options:
  --dry-run   Don't export anything; just check each table exists and
              print its row count.
  -h, --help  Show this help and exit.

Examples:
  $0 fg-production-sandbox-46.finngen_omop_result_dev gs://mytmpbucket/tmp
  $0 --dry-run fg-production-sandbox-46.finngen_omop_result_dev gs://mytmpbucket/tmp
EOF
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  usage
  exit 0
fi

DRY_RUN=false
if [ "${1:-}" = "--dry-run" ]; then
  DRY_RUN=true
  shift
fi

if [ "$#" -ne 2 ]; then
  usage >&2
  exit 1
fi

PROJECT_DATASET="$1"
GCS_OUTPUT_PATH="${2%/}"

case "${GCS_OUTPUT_PATH}" in
  gs://*) ;;
  *)
    echo "Error: gcs_output_path must start with gs:// (got '${GCS_OUTPUT_PATH}')" >&2
    exit 1
    ;;
esac

# project.dataset -> project (before the first dot) and dataset (after it).
# `bq show`/`bq extract` need the colon form (project:dataset.table).
PROJECT="${PROJECT_DATASET%%.*}"
DATASET="${PROJECT_DATASET#*.}"

TABLES=("stratified_code_counts" "stratified_persons" "code_counts")

if [ "${DRY_RUN}" = true ]; then
  for TABLE in "${TABLES[@]}"; do
    TABLE_REF="${PROJECT}:${DATASET}.${TABLE}"
    if SHOW_JSON=$(bq show --format=json "${TABLE_REF}" 2>&1); then
      NUM_ROWS=$(printf '%s' "${SHOW_JSON}" | python3 -c "import json,sys; print(json.load(sys.stdin)['numRows'])")
      echo "[${TABLE}] exists, ${NUM_ROWS} rows"
    else
      echo "[${TABLE}] NOT FOUND: ${SHOW_JSON}" >&2
    fi
  done
  exit 0
fi

for TABLE in "${TABLES[@]}"; do
  TABLE_REF="${PROJECT}:${DATASET}.${TABLE}"
  TABLE_GCS_PREFIX="${GCS_OUTPUT_PATH}/${TABLE}"

  echo "[${TABLE}] exporting schema..."
  bq show --schema --format=prettyjson "${TABLE_REF}" \
    | gsutil cp - "${TABLE_GCS_PREFIX}/schema.json"

  echo "[${TABLE}] extracting data to GCS..."
  bq extract \
    --destination_format=CSV \
    --field_delimiter=',' \
    --print_header=true \
    "${TABLE_REF}" \
    "${TABLE_GCS_PREFIX}/${TABLE}-*.csv"
done

echo "Done. Output written to ${GCS_OUTPUT_PATH}"
