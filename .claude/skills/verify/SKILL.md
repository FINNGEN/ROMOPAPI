# Verifying ROMOPAPI at runtime

Repo is a single R package (plumber API). No config needed to boot — it
ships a bundled Eunomia/FinnGen-counts-only SQLite test DB.

## Build + launch

```bash
cd <repo root>
lsof -nP -iTCP:8564 -sTCP:LISTEN   # check for a stale server first; kill its PID if found
Rscript -e 'source("renv/activate.R"); pkgload::load_all(".", quiet = TRUE); ROMOPAPI::runApiServer()' \
  > /tmp/verify_server.log 2>&1 &
sleep 10
curl -s -o /dev/null -w "%{http_code}\n" http://127.0.0.1:8564/__docs__/   # 200 once up
```

`pkgload::load_all` picks up uncommitted source changes without a full
install. `source("renv/activate.R")` is required — a plain `Rscript` doesn't
source `.Rprofile`, so the renv library never activates and package loads
fail. When done: `kill <pid>` (found via the `lsof` line above), then confirm
the port is free again.

## Endpoints worth driving

All defined in `inst/plumber/plumber.R`. Known-good example ID: `conceptId=317009`
(Asthma) — has real data in the bundled fixture.

- `/getListOfConcepts` — concept_id, concept_name, vocabulary_id, concept_code only.
- `/getConceptRelationships?conceptId=` → `{concept_relationships, concepts}`.
- `/getCodeCountsStratified?conceptId=` → flat array of per-stratum rows.
- `/getPersonCountsFilters?conceptIds=317009SD&yearsRange=2015,2020` → sex/age/visit/year breakdown.
  `conceptIds` is a comma-separated list of tagged tokens (`<conceptId><S|M><D?>`).
- `/getPersonCountsUpset?conceptIds=317009SD&yearsRange=&sexStratum=&ageStratum=&visitStratum=`
  → UpSet exclusive-region counts, one region per combination of `conceptIds` tokens.
- `/report?conceptId=` → full HTML report (mermaid tree + tables + 5 plotly
  widgets: code-counts plot + sex pie + age histogram + visit barplot + upset).

## Known gotchas

- A project-level `.Renviron` hardcodes `HADESEXTAS_TESTING_ENVIRONMENT` —
  shell-exporting it before `Rscript` does **not** override it (R's startup
  `.Renviron` processing runs after and wins). To force a testthat DB stage,
  call `Sys.setenv(HADESEXTAS_TESTING_ENVIRONMENT = "...")` **inside** the R
  script, after `source("renv/activate.R")`.
- `R CMD check` on a built tarball spawns a subprocess that never sources
  `.Rprofile`, so `renv::activate()` doesn't run and "package not available"
  errors follow. Pass `R_LIBS_USER=<renv lib path>` and
  `--library=<same path>` explicitly. Find the path with
  `Rscript -e 'cat(renv::paths$library())'`.
- `/report`'s plumber handler has **no `tryCatch`** around `createReport()`
  (unlike every other endpoint) — a conceptId with zero data (e.g. an ID not
  in the vocabulary at all) 500s with a bare "Internal server error" instead
  of a clean 400. Pre-existing, not specific to any one change; worth knowing
  before blaming a new diff for it.
- Malformed non-numeric query params that get `as.integer()`'d produce `NA`,
  which downstream `checkmate::assertIntegerish(..., null.ok = TRUE)` calls
  happily accept (NA passes "integerish"). The failure then surfaces later as
  either a confusing low-level R error (`"missing value where TRUE/FALSE
  needed"` from an `if (NA)`) or a raw SQL error (`"no such column: NA"`)
  rather than a clean validation message — still a 400 via the endpoint's
  outer `tryCatch`, just not a helpful one.
