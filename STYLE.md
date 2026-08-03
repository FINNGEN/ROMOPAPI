# R Package Style Guide

## Pipe operator

- Always use `|>` (native pipe) instead of `%>%` (magrittr pipe).

## Roxygen documentation

- Use Roxygen2 for all function documentation.
- Always include `@importFrom` directives for functions imported from other
  packages.
- Do NOT include `@examples` sections.
- Standard format: `@title`, `@description`, `@param`, `@return`,
  `@importFrom package function`.
- `@export` is only used for functions that are intended to be used by the
  user. Internal functions should not be exported and must start with a dot.

## Package preferences

- Prefer tidyverse functions over base R when possible.
- Use `dplyr`, `readr`, `tibble`, `stringr`, `purrr` instead of base R
  equivalents.
- Use base R when it's more appropriate or when tidyverse adds unnecessary
  dependencies.

## Code style

- Use camelCase: function and variable names start with lowercase, package
  names start with uppercase.
- Function names are verbs, variable names are nouns
  (e.g. `fitModel`, `population`).
- Use `<-` for assignment, not `=`.
- Place spaces around infix operators (`=`, `+`, `-`, `<-`, etc.) and after
  commas.
- Always use curly braces `{}` for if-then-else, even single statements.
- Use named arguments when calling functions with more than one argument.
- Pipes (`|>`) should be at the end of the line.
- Limit lines to 100 characters.
- Comments explain *why*, not *what*; use `#` with a space.