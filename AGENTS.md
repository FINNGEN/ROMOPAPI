# AGENTS.md — ROMOPAPI

ROMOPAPI is an **R package** exposing a [plumber](https://www.rplumber.io/) API
that serves code counts and concept reports from OMOP CDM databases. It follows
the standard R package layout, and ships with a `Dockerfile` so it can be
containerised for production. Read this before making changes.

The database is an OMOP Common Data Model; SQL is written for OHDSI/HADES tooling
and run through `SqlRender` (use OMOP CDM tables/columns in queries). It supports
local SQLite (Eunomia) and remote connections (BigQuery) via DBI.

## Layout

Standard R package structure:

- `R/` — package source, one file per function group. The plumber router and
  endpoints are in `R/api.R`; the rest are named after what they do
  (`getCodeCounts.R`, `createReport.R`, `createStratifiedCodeCountsTable.R`,
  `hllCount.R`, …).
- `man/` — generated roxygen docs (don't hand-edit; run `devtools::document()`).
- `tests/testthat/` — automated tests. `tests/testmanual/` — manual scripts, not
  run in CI.
- `inst/` — installed files: `inst/sql/` (query templates), `inst/reports/` (the
  R Markdown report served by `/report`), `inst/plumber/`, `inst/testdata/`.
- `vignettes/`, `DESCRIPTION`, `NAMESPACE` — package metadata and docs as usual.

SqlRender structure (`inst/sql/`):

- SQL is written once in the **SQL Server** dialect (SqlRender's source dialect)
  under `inst/sql/sql_server/` — this is the canonical folder.
- At runtime the code picks a dialect folder from `connection@dbms`
  (`inst/sql/bigquery/` for BigQuery, `inst/sql/sql_server/` otherwise), then
  passes the SQL through `SqlRender::translate()` to the actual target (e.g.
  sqlite). See `R/createCodeCountsTables.R`.
- A dialect folder like `bigquery/` exists only to override the few queries where
  automatic translation isn't enough (e.g. HLL sketches). It is **not** a mirror
  copy — change the `sql_server` version by default and only add/edit a dialect
  override when that platform genuinely needs different SQL.

## Languages and styling

Full rules are in **STYLE.md** — read it before writing code.

- **R** — camelCase, native pipe `|>`, tidyverse-first, roxygen2 for all docs.
  See STYLE.md.
- **SQL** — OHDSI SQL Server dialect + SqlRender (`@param` placeholders,
  `render()`/`translate()`). SQL conventions are in STYLE.md under "SQL".
- **Docker** — a single multi-stage-ish `Dockerfile` (`rocker/r-ver` base). Keep
  it minimal; see Security instructions for the secret handling that must not
  change.

@STYLE.md

## Dependencies

- Declared in `DESCRIPTION`; `renv.lock` pins the exact versions.
- `DatabaseConnector` in `Remotes:` points to a fork
  (`javier-gracia-tabuenca-tuni/DatabaseConnector@bigquery-DBI-2`), **not** CRAN.
  This is intentional — it adds BigQuery DBI support. Don't "fix" it back to the
  upstream package.
- BigQuery support also pulls in `bigrquery` (Suggests) and needs Java at runtime
  (installed in the Dockerfile for `rJava`/`DatabaseConnector`).

## Development instructions

- Run the API locally with `ROMOPAPI::runApiServer()` — starts on port 8564 using
  the bundled Eunomia test database, no config needed. Try
  `GET /report?conceptId=317009`.
- For a custom database, pass `cohortTableHandlerConfig` (a parsed
  `database_config.yml`) and add `buildCountsTable = TRUE` on first run. See
  `README.md` for the full config example.
- After changing exported functions or their roxygen, run `devtools::document()`
  so `man/` and `NAMESPACE` stay in sync.
- For larger or riskier issues (DB connection code, many files, or anything under
  Security instructions), use the `my-plan` skill first: post a plan as an issue
  comment and wait for human approval before writing code. Small, well-scoped
  issues can go straight to implementation.

## Git strategy

- Feature branches → PR into `development`. Never push directly to `development`
  or `main`.
- Only a human opens the `development` → `main` PR (production release). Don't
  attempt this yourself even if asked to "finish the release."
- Link the PR to the issue it resolves (`Fixes #<n>`).
- If an issue is large, commit in small logical chunks rather than one huge diff.
- Use the `commit` skill (`.claude/skills/commit/`) to stage and commit — it
  applies the repo's Conventional Commits format and safety checks.

## Testing instructions

- Run tests with `devtools::test()` (testthat edition 3) before opening a PR.
- Tests live in `tests/testthat/`; scripts in `tests/testmanual/` are manual and
  are not run in CI — don't rely on them for coverage.
- Tests use the bundled Eunomia/test data (`inst/testdata/`); they must not
  require a live remote database.

## Building instructions

- Run `R CMD check` (or `devtools::check()`) for anything touching exported
  functions or `DESCRIPTION`. CI runs this too (`R-CMD-check` on PRs to `main`
  and `development`), but catch it locally/in-session first when you can.
- Container image: build with the GitHub PAT passed as a build secret —
  `docker build --secret id=build_github_pat,src=GITHUBPAT.txt -t romopapi .`
  The image installs the package via `renv` and runs `runApiServer()` on 8564.
  See `README.md` for build args (`ROMOPAPI_BRANCH`, `BUILD_CACHE_BUSTER`).

## Security instructions

Do not touch the following without a human explicitly asking:

- Database connection / credential handling (`database_config.yml`,
  `GCP_SERVICE_KEY`, anything under `bigrquery::bq_auth`).
- Docker secret handling in the `Dockerfile` (`GITHUBPAT`,
  `--mount=type=secret`). `GITHUBPAT.txt` is gitignored — keep it that way.
- Never write credentials, tokens, or keys into any file, log, or commit.
