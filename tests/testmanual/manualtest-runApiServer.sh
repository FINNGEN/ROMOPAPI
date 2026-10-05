#!/usr/bin/env bash
# Run the ROMOPAPI plumber API server against the bundled Eunomia test database,
# loading the package from source (pkgload::load_all) so local edits are picked
# up without a full install. Ctrl-C to stop.
#
# Usage: tests/testmanual/manualtest-runApiServer.sh

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"

cd "${repo_root}"

Rscript -e '
  pkgload::load_all(".", quiet = TRUE)
  runApiServer(
    cohortTableHandlerConfig = NULL,
    buildCountsTable = FALSE
  )
'
