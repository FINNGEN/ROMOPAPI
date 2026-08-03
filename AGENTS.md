# AGENTS.md — ROMOPAPI

R package (plumber API) that serves code counts and concept reports from OMOP CDM
databases. Read this before making changes.

## Build & test

- Run tests with `devtools::test()` (testthat edition 3) before opening a PR.
- Run `R CMD check` (or `devtools::check()`) for anything touching exported
  functions or the DESCRIPTION file — CI runs this too, but catch it locally/
  in-session first when you can.
- If an issue is large, commit in small logical chunks rather than one huge diff.

## Things that look like mistakes but aren't

- `DatabaseConnector` in `Remotes:` points to a fork
  (`javier-gracia-tabuenca-tuni/DatabaseConnector@bigquery-DBI-2`), not CRAN.
  This is intentional (BigQuery DBI support). Don't "fix" it back to the
  upstream package.

## Do not touch without a human explicitly asking

- Database connection / credential handling (`database_config.yml`,
  `GCP_SERVICE_KEY`, anything under `bigrquery::bq_auth`).
- Docker secret handling in the `Dockerfile` (`GITHUBPAT`, `--mount=type=secret`).
- Never write credentials, tokens, or keys into any file, log, or commit.

## Branching & PRs

- Feature branches → PR into `development`. Never push directly to
  `development` or `main`.
- Only a human opens the `development` → `main` PR (production release). Don't
  attempt this yourself even if asked to "finish the release."
- Link the PR to the issue it resolves (`Fixes #<n>`).


## Style

Full rules below.
/@STYLE.md

## Planning before implementing

- For larger or riskier issues (touching DB connection code, many files, or
  anything under "do not touch" above), use the `my-plan` skill first: post a
  plan as an issue comment and wait for human approval before writing code.
- Small, well-scoped issues (a clear bug fix, a small doc update) can go
  straight to implementation.