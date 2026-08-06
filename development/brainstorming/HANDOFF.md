# ROMOPAPI — Handoff

## What it is

R package. Plumber-based HTTP API serving OMOP CDM data. Wraps a CDM database
(SQLite / BigQuery / Postgres via Hades `DatabaseConnector`), exposes endpoints
that return a concept hierarchy tree plus event counts per node.

Primary consumer: clients that query by `conceptId` and want the hierarchy of
parents, descendants, and mapped concepts together with counts.

## Repo layout

```
R/                       package source
  runApiServer.R         entry point — starts Plumber on host:port
  api.R                  Plumber router (legacy `create_api`)
  createCodeCountsTables.R         builds aggregate counts table
  createStratifiedCodeCountsTable.R builds per-stratum counts table
  getCodeCounts.R        main query: tree + counts for a conceptId
  getConceptsWithCodeCounts.R       cache of available concepts
  getVisitTypeNames.R    visit-group label lookup
  getAPIInfo.R / getLogs.R / sendFeedback.R
  createReport.R         HTML report rendering
  plotingFunctions.R     plots used in reports
  helper.R / HadesExtras.R / fct_logs.R    config, CDM handler, logging
inst/
  plumber/plumber.R      Plumber endpoint definitions
  sql/sql_server/        Hades-flavoured SQL templates
  reports/               report assets (incl. mermaid.min.js)
  testdata/              test configs + Eunomia sqlite path
  scripts/               dev scripts
dev/
  1_start.R, 03_deploy.R dev entry scripts
  brainstorming/         design notes (HyperLogLog.md, this file)
man/                     roxygen-generated Rd
tests/testthat/          unit tests
vignettes/               package vignettes
Dockerfile               container build
renv.lock / renv/        pinned deps
```

## How code counts are produced

### 1. Pre-creation of count tables (`buildCountsTable = TRUE`)

Driver: `createCodeCountsTables()` in `R/createCodeCountsTables.R`.

**Step 1a — build `stratified_code_counts`** (atomic per-stratum events).

`createStratifiedCodeCountsTable()` (`R/createStratifiedCodeCountsTable.R`)
loops over 7 OMOP domains:

| domain | source table | concept field | date field | source concept field |
|--------|--------------|---------------|------------|----------------------|
| Condition | `condition_occurrence` | `condition_concept_id` | `condition_start_date` | `condition_source_concept_id` |
| Procedure | `procedure_occurrence` | `procedure_concept_id` | `procedure_date` | `procedure_source_concept_id` |
| Drug | `drug_exposure` | `drug_concept_id` | `drug_exposure_start_date` | `drug_source_concept_id` |
| Measurement | `measurement` | `measurement_concept_id` | `measurement_date` | `measurement_source_concept_id` |
| Observation | `observation` | `observation_concept_id` | `observation_date` | `observation_source_concept_id` |
| Device | `device_exposure` | `device_concept_id` | `device_exposure_start_date` | `device_source_concept_id` |
| Visit | `visit_occurrence` | `visit_concept_id` | `visit_start_date` | `visit_source_concept_id` |

For each domain, SQL template
`inst/sql/sql_server/appendToStratrifiedCodeCountsTable.sql` runs:
- JOIN domain table → `person` (age, gender) → `observation_period` (validity
  filter).
- If `visitSourceGroupConceptIds != 0`: LEFT JOIN `visit_occurrence` and a
  `concept_ancestor` subquery to map each event to a visit-group ancestor.
- Filter `concept_id != 0`.
- `GROUP BY` `(concept_id, maps_to_concept_id, visit_group_concept_id,
  calendar_year, gender_concept_id, age_decile)`.
- `COUNT_BIG(*)` → `record_counts`.

Input columns used:
- `@cdmDatabaseSchema.<domain_table>`: `person_id`, `<concept_id_field>`,
  `<maps_to_concept_id_field>`, `<date_field>`, `visit_occurrence_id`
  (visit grouping only).
- `@cdmDatabaseSchema.person`: `person_id`, `gender_concept_id`,
  `year_of_birth`.
- `@cdmDatabaseSchema.observation_period`: `person_id`,
  `observation_period_start_date`, `observation_period_end_date`.
- `@cdmDatabaseSchema.visit_occurrence` (visit grouping only):
  `visit_occurrence_id`, `visit_source_concept_id`.
- `@cdmDatabaseSchema.concept_ancestor` (visit grouping only):
  `ancestor_concept_id`, `descendant_concept_id`.

Output table `@resultsDatabaseSchema.stratified_code_counts` columns:

| column | type | notes |
|--------|------|-------|
| `concept_id` | INTEGER | standard concept |
| `maps_to_concept_id` | INTEGER | source concept |
| `visit_group_concept_id` | INTEGER | `0` if no grouping |
| `calendar_year` | INTEGER | `YEAR(date_field)` |
| `gender_concept_id` | INTEGER | from `person` |
| `age_decile` | INTEGER | `FLOOR((year - year_of_birth)/10)` |
| `record_counts` | INTEGER | `COUNT_BIG(*)` |

Result: one INSERT per domain. Table dropped+recreated each call.

**Step 1b — build `code_counts`** (aggregate, with descendant rollup).

SQL template `inst/sql/sql_server/createCodeCountsTable.sql` aggregates
`stratified_code_counts` over all strata, then joins `concept_ancestor` to
roll descendant counts up the hierarchy.

Inputs:
- `@resultsDatabaseSchema.stratified_code_counts`: `concept_id`,
  `maps_to_concept_id`, `visit_group_concept_id`, `calendar_year`,
  `gender_concept_id`, `age_decile`, `record_counts`.
- `@cdmDatabaseSchema.concept_ancestor`: `ancestor_concept_id`,
  `descendant_concept_id`, `min_levels_of_separation`,
  `max_levels_of_separation` (self-edges are union-injected in a CTE).
- `@cdmDatabaseSchema.concept`: `concept_id` (used to inject self-edges).

Output table `@resultsDatabaseSchema.code_counts` columns:

| column | type | notes |
|--------|------|-------|
| `concept_id` | BIGINT | |
| `record_counts` | BIGINT | sum of `stratified_code_counts.record_counts` for this concept (including `maps_to` rollup, excluding maps-to-self) |
| `descendant_record_counts` | BIGINT | sum of `record_counts` over all `concept_ancestor` descendants |
| `number_of_descendants` | BIGINT | count of descendants |

Note: the roxygen / README description of `code_counts` mentions
`person_counts`, `incidence_person_counts`, `descendant_person_counts`,
`total_person_counts` etc. — the current SQL does **not** emit those;
only the four columns above are written. Docs are stale.

### 2. Runtime — `GET /getCodeCounts?conceptId=`

Entry: `getCodeCounts()` in `R/getCodeCounts.R` (memoised via
`getCodeCounts_memoise`).

For a request, data sources are:

- **Hierarchy tree** (parents / descendants / mapped) →
  `@vocabularyDatabaseSchema.concept_ancestor`. Inline SQL in
  `getCodeCounts.R` lines 58–112. Columns read:
  `ancestor_concept_id`, `descendant_concept_id`,
  `min_levels_of_separation`. Filtered to concepts that also exist in
  `stratified_code_counts` (via `concept_id` / `maps_to_concept_id`) so the
  tree is pruned to nodes with data.
- **Per-stratum counts** for those tree nodes →
  `@resultsDatabaseSchema.stratified_code_counts`. SELECT at
  `getCodeCounts.R:159`, filtered `concept_id IN (...) OR maps_to_concept_id
  IN (...)`. Columns read: all 7
  (`concept_id`, `maps_to_concept_id`, `visit_group_concept_id`,
  `calendar_year`, `gender_concept_id`, `age_decile`, `record_counts`).
- **`Maps to` / `Mapped from` edges** → derived in R from the
  `(concept_id, maps_to_concept_id)` pairs in the same
  `stratified_code_counts` rows (no extra SQL).
- **Concept names / metadata** →
  `getConceptsWithCodeCounts_memoise()` (`R/getConceptsWithCodeCounts.R`),
  which JOINs `@vocabularyDatabaseSchema.concept` with
  `@resultsDatabaseSchema.code_counts`. Columns read:
  - `concept`: `concept_id`, `concept_name`, `domain_id`, `vocabulary_id`,
    `concept_class_id`, `standard_concept`, `concept_code`.
  - `code_counts`: `concept_id`, `record_counts`,
    `descendant_record_counts`, `number_of_descendants`.

  Populated into cache at server startup by `runApiServer()`.
- **Descendant rollup** (`node_descendant_record_counts`) → computed in R
  client-side: walks the tree with `.familyTreeToAncestorTable()` then sums
  `record_counts` across each node's descendants via `dplyr`. The
  `code_counts.descendant_record_counts` column is **not** used for this
  per-request rollup — it is only surfaced as a global metric via the
  `concepts` tibble.

Response = list of 3 tibbles.

**`concept_relationships`** — tree edges + mapping edges. One row per edge.

| column | type | notes |
|--------|------|-------|
| `parent_concept_id` | int | edge tail |
| `child_concept_id` | int | edge head |
| `levels` | chr | `"-1"` (parent of root), `"0"` (root self-edge), `"n-n"` (n hops below parent), `"Mapped from"`, `"Maps to"` |
| `concept_class_id` | chr | class of child, joined from `concepts` (e.g. `"Clinical Finding"`, `"ICD10 code"`) |

**`stratified_code_counts`** — counts per node × stratum after rollup.

| column | type | notes |
|--------|------|-------|
| `concept_id` | int | tree node |
| `visit_group_concept_id` | int | `0` if no grouping |
| `calendar_year` | int | |
| `gender_concept_id` | int | |
| `age_decile` | int | |
| `node_record_counts` | int | events on this concept only (`0` if none) |
| `node_descendant_record_counts` | int | sum over node + all descendants in returned tree |

**`concepts`** — concept metadata + global counts. One row per distinct
`child_concept_id` in the tree (incl. mapped concepts).

| column | type | notes |
|--------|------|-------|
| `concept_id` | dbl | |
| `concept_name` | chr | `"Missing concept Name"` if absent in vocab (Eunomia fallback) |
| `domain_id` | chr | `"NA"` fallback |
| `vocabulary_id` | chr | `"NA"` fallback |
| `concept_class_id` | chr | `"NA"` fallback |
| `standard_concept` | lgl | `TRUE` fallback |
| `concept_code` | chr | `"NA"` fallback |
| `record_counts` | int | from `code_counts` (global, all strata) |
| `descendant_record_counts` | int | from `code_counts` (global, all descendants in vocab) |

## Data model (results schema)

Two tables built by `createCodeCountsTables()`:

1. `stratified_code_counts` — atomic, one row per
   `(concept_id, maps_to_concept_id, visit_group_concept_id, calendar_year,
   gender_concept_id, age_decile)`. Column `record_counts` = `COUNT(*)` of
   events in domain tables (condition_occurrence, drug_exposure, ...) restricted
   to valid observation periods. Built per domain by
   `inst/sql/sql_server/appendToStratrifiedCodeCountsTable.sql`.

2. `code_counts` — aggregate built from the stratified table by
   `inst/sql/sql_server/createCodeCountsTable.sql`. Adds person-level columns
   (`person_counts`, `incidence_person_counts`, `descendant_*`,
   `total_person_counts`).

`visit_group_concept_id` is `0` unless `visitSourceGroupConceptIds` is passed
to `runApiServer`, in which case events are mapped via `concept_ancestor` to
their group.

## API endpoints (port 8564 default)

Defined in `inst/plumber/plumber.R`:

| Method | Path | Purpose |
|--------|------|---------|
| GET | `/health` (in api.R) | health check |
| GET | `/echo` | echo |
| GET | `/getCodeCounts?conceptId=` | concept tree + counts (memoised) |
| GET | `/getAPIInfo` | database / build info |
| GET | `/getListOfConcepts` | concepts present in counts tables |
| GET | `/report?conceptId=&showsMappings=&pruneLevels=&pruneClass=` | HTML report |
| GET | `/getVisitTypeNames` | visit-group label lookup |
| GET | `/getLogs` | server logs |
| POST | `/sendFeedback` | feedback drop |
| GET | `/mermaid.min.js` | static asset for report |

`/getCodeCounts` response shape: list of three tibbles
`concept_relationships`, `stratified_code_counts`, `concepts` (full schema in
README).

CORS open (`*`). Memoisation wraps `getCodeCounts`, `getConceptsWithCodeCounts`,
`getVisitTypeNames`.

## How to run

Default (bundled FinnGen Eunomia sqlite, counts pre-built):
```r
ROMOPAPI::runApiServer()
```

Custom DB — needs a yaml config (see README §"custom database"). First run must
build counts:
```r
config <- yaml::read_yaml("database_config.yml")
ROMOPAPI::runApiServer(
  cohortTableHandlerConfig = config$cohortTableHandler,
  buildCountsTable = TRUE,
  visitSourceGroupConceptIds = 0   # or vector of visit group ancestor IDs
)
```

Docker image at `javiergrata/romopapi`. Build needs `GITHUBPAT.txt` in repo
root (see Dockerfile).

## Conventions

From `llms.md`:
- Native pipe `|>`, not `%>%`.
- Roxygen2 for all functions; no `@examples` blocks per project rule (existing
  files violate this — pre-existing).
- camelCase, `<-` assignment, braces always.
- Tidyverse over base R where appropriate.
- SQL = Hades flavour via `SqlRender::render`/`translate`.
- Internal (non-exported) functions start with `.`.

## Recent state

Branch `person_counts`. Recent commits add
`visitSourceGroupConceptIds` plumb-through from `runApiServer` →
`createCodeCountsTables` → `createStratifiedCodeCountsTable`. PRs #33, #34
merged the hospital-stratification work into main.

## Open design threads

`dev/brainstorming/HyperLogLog.md` — proposes HLL sketches (p=10, 48-bit hash,
1024 buckets, ~768 B) to enable distinct-person rollups across the concept
descendant tree without double-counting, and cross-database union without
sharing person_ids. Current counts are event-level `COUNT(*)` summed across
descendants, which is correct for events but not for distinct persons.

Open questions:
- Sketch granularity: per concept, or per stratum cell (cardinality explodes
  with full stratification).
- Backend support: BigQuery has native `HLL_COUNT.MERGE`; SQLite does not.
  Likely merge in R client-side for cross-dialect support.
- Small-cell error at p=10 (~3.25% relative). HLL++ sparse mode if needed.
- Replace `record_counts` or add a parallel sketch column.

## Gotchas

- `GITHUBPAT.txt` lives at repo root and is required for Docker builds — keep
  out of commits (it is in `.gitignore` territory; do not stage).
- `buildCountsTable = TRUE` is destructive: drops and recreates
  `stratified_code_counts` (and `code_counts`).
- Counts SQL filters events to within `observation_period`; events outside are
  silently dropped.
- `concept_id != 0` filter in stratified SQL — events with no concept mapping
  are excluded.
- `age_decile = floor((year(date) - year_of_birth) / 10)` — bucket by decade,
  not standard age groups.
- `visit_group_concept_id = 0` is the sentinel for "no grouping applied";
  passing a non-zero `visitSourceGroupConceptIds` changes table semantics.
