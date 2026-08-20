# CLAUDE.md — ROMOPAPI

ROMOPAPI is an **R package** exposing a [plumber](https://www.rplumber.io/) API
that serves precomputed code counts and concept reports from an OMOP CDM database
(local SQLite/Eunomia or remote BigQuery, via DBI). Standard R package layout;
ships a `Dockerfile` for production.

**For the architecture — what the parts are and how they interact — read
[development/STRUCTURE.md](development/STRUCTURE.md) first.** This file (CLAUDE.md)
covers how to *work* in the repo: style, tests, build, deploy, git, security.

@development/STRUCTURE.md

## Layout

Standard R package. Beyond the module map in STRUCTURE.md, note:

- `man/` — generated roxygen docs (don't hand-edit; run `devtools::document()`).
- `tests/testthat/` — CI tests. `tests/testmanual/` — manual, not run in CI.
- `inst/` — installed files: `sql/` templates, `reports/` (the `/report` Rmd),
  `plumber/` (the live router), `testdata/`.

SQL lives in `inst/sql/`. Write each template once in the **SQL Server** dialect
under `inst/sql/sql_server/` (canonical); `SqlRender::translate()` converts it at
runtime. Add a dialect folder (`inst/sql/bigquery/`) **only** to override the few
queries translation can't handle — edit `sql_server` by default, never keep a
full divergent copy.

## Languages and styling

Full rules in **[development/STYLE.md](development/STYLE.md)** — read before writing code.

- **R** — camelCase, native pipe `|>`, tidyverse-first, roxygen2 for all docs.
- **SQL** — OHDSI SQL Server + SqlRender: `@param` placeholders, snake_case
  aliases (BigQuery folds identifiers to lowercase, so camelCase comes back
  mangled).
- **Docker** — single `Dockerfile` (`rocker/r-ver`). Keep minimal; don't touch
  secret handling (see Security).

@development/STYLE.md

## Dependencies

- Declared in `DESCRIPTION`, pinned in `renv.lock`.
- `DatabaseConnector` is a **fork**
  (`javier-gracia-tabuenca-tuni/DatabaseConnector@bigquery-DBI-2`) that adds
  BigQuery DBI support — intentional, don't revert it to CRAN.
- BigQuery also needs `bigrquery` (Suggests) and Java at runtime (in the Dockerfile).

## Development

- Run locally: `ROMOPAPI::runApiServer()` — port 8564, bundled Eunomia DB, no
  config. Try `GET /report?conceptId=317009`.
- Custom DB: pass `cohortTableHandlerConfig` (parsed `database_config.yml`) plus
  `buildCountsTable = TRUE` on first run. See `README.md`.
- After changing exported functions or their roxygen: run `devtools::document()`.
- Real-DB tests need `EUNOMIA_DATA_FOLDER` and `GCP_SERVICE_KEY` in `~/.Renviron`;
  without them the Eunomia/BigQuery tests skip (expected on CI).

## Testing

- Run `devtools::test()` (testthat edition 3) before a PR.
  `HADESEXTAS_TESTING_ENVIRONMENT` selects the target DB (`tests/testthat/setup.R`).
- Each testing DB has **one job** — a test runs only against its stage and skips
  otherwise (`skip_if(testingDatabase != ...)`):

| Database | dbms | Use it for — and *only* this |
|----------|------|------------------------------|
| **Eunomia-GiBleed** | sqlite | Counts-table **creation** from a raw OMOP CDM. |
| **OnlyCounts-FinnGen** | sqlite | Functions that run **after** counts tables exist (ships them precomputed in `inst/testdata/data/FinnGenR13_countsOnly.sqlite`). |
| **AtlasDevelopment-5k** | BigQuery | **Both** on BigQuery: creation into a throwaway temp table (dropped after), and post-counts functions. Subset of AtlasDevelopment-full. |
| **AtlasDevelopment-full** | BigQuery | Regenerating the OnlyCounts-FinnGen fixture only (`inst/testdata/data/createTestingData.R`). Not in regular runs. |

- Creation tests → Eunomia-GiBleed + AtlasDevelopment-5k; post-counts tests →
  OnlyCounts-FinnGen + AtlasDevelopment-5k. Nothing else.

### Database-dependent settings

- `visitSourceGroupConceptIds` (FinnGen visit-source-group IDs) is
  **database-dependent** — never hardcode it in function bodies or tests. It
  belongs in `databasesConfig.yml` per database (the FinnGen list for the
  FinnGen/BigQuery databases, empty/`0` for Eunomia).
- Default `0` disables grouping: the SQL is guarded with
  `{@visit_group_concept_ids != 0} ? {…} : {0}`, so the setting is harmless when
  empty.

## Building

- Run `R CMD check` / `devtools::check()` for anything touching exported functions
  or `DESCRIPTION` (CI runs it on PRs to `main`/`development` — catch it locally too).
- Image:
  `docker build --secret id=build_github_pat,src=GITHUBPAT.txt -t romopapi .`
  Installs via `renv`, runs `runApiServer()` on 8564. Build args in `README.md`.

## Deploy

The API serves precomputed counts, so a deployment is two steps — build, then serve:

- **First run / new DB / after a CDM or counts-logic change** —
  `runApiServer(..., buildCountsTable = TRUE)` to (re)build the counts tables.
- **Normal serving** — `buildCountsTable = FALSE` (the default); reads the
  existing tables, no rebuild.

## Git

- Feature branch → PR into `development`. Never push directly to `development`/`main`.
- Only a human opens the `development` → `main` (release) PR — don't attempt it,
  even if asked to "finish the release."
- Link the PR to its issue (`Fixes #<n>`); commit in small logical chunks.
- Only commit when the user explicitly asks — never commit proactively. The
  diffs must be evaluated first, so leave changes unstaged until then.
- Use the `commit` skill (`.claude/skills/commit/`) — it applies the repo's
  Conventional Commits format and safety checks.

## Security

Do not touch the following without a human explicitly asking:

- DB connection / credential handling (`database_config.yml`, `GCP_SERVICE_KEY`,
  anything under `bigrquery::bq_auth`).
- Docker secret handling in the `Dockerfile` (`GITHUBPAT`, `--mount=type=secret`).
  `GITHUBPAT.txt` is gitignored — keep it that way.
- Credentials live outside the repo in `~/.Renviron` (`EUNOMIA_DATA_FOLDER`,
  `GCP_SERVICE_KEY`) — never read, print, or copy their values into any file, log,
  or commit.
