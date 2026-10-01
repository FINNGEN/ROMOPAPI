# sandbox

## export_precomputed_tables.sh

Exports the three ROMOPAPI precomputed counts tables
(`stratified_code_counts`, `stratified_persons`, `code_counts`) from a
BigQuery dataset to GCS: one folder per table, each with the table schema
(`schema.json`) and the data as CSV shard(s) (empty string for NULL cells).

Uses `bq extract` (a server-side BigQuery job) rather than `bq query`, so it
stays fast on large production tables instead of paging rows through the CLI.

```
./export_precomputed_tables.sh [--dry-run] <project.dataset> <gcs_output_path>
```

- `project.dataset` — BigQuery project + dataset holding all 3 tables, e.g.
  `atlas-development-270609.finngen_omop_results_dev_5k`
- `gcs_output_path` — destination GCS path, e.g. `gs://mytmpbucket/tmp`
- `--dry-run` — skip the export; just check each table exists and print its
  row count
- `-h` / `--help` — full usage

Requires `bq`/`gsutil` (gcloud SDK) authenticated against the target project.

### Examples

Build environment (`AtlasDevelopment-5k`):

```
bash export_precomputed_tables.sh \
  atlas-development-270609.finngen_omop_results_dev_5k \
  gs://mytmpbucket/tmp
```

Preview environment:

```
bash export_precomputed_tables.sh \
  fg-production-sandbox-46.finngen_omop_result_dev \
  gs://fg-production-sandbox-46-red/JAVIER/tmp
```
