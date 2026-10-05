#' Parse a tagged concept-ids string into one row per set
#'
#' @description
#' Parses a comma-separated list of tagged concept references, one row per
#' token, into the (concept id, matched column, expand-descendants) triple
#' each tag encodes. Tag grammar: `<conceptId><S|M><D?>` — `S` matches
#' `concept_id`, `M` matches `maps_to_concept_id`; a trailing `D` expands the
#' set to the concept and all its descendants instead of the code alone.
#' Shared by `getPersonCountsUpset()` and `getPersonCountsFilters()`.
#'
#' @param conceptIdsString Comma-separated tagged concept ids, e.g.
#'   `"317009S,4191479SD,2000403993M,2000403993MD"`.
#'
#' @return A tibble of `token`, `concept_id`, `column`
#'   (`"concept_id"`/`"maps_to_concept_id"`), `expand_descendants` (logical).
#'
#' @importFrom tibble tibble
#'
.parsePersonCountsConceptIds <- function(conceptIdsString) {
    tokens <- conceptIdsString |>
        trimws() |>
        strsplit(",", fixed = TRUE) |>
        (\(x) x[[1]])() |>
        trimws()

    if (length(tokens) == 0 || any(!nzchar(tokens))) {
        stop("conceptIds must be a non-empty comma-separated list of tagged concept ids")
    }

    pattern <- "^([0-9]+)([SM])(D)?$"
    matches <- regmatches(tokens, regexec(pattern, tokens))

    invalid <- tokens[lengths(matches) == 0]
    if (length(invalid) > 0) {
        stop(
            "invalid conceptIds token(s): ", paste(invalid, collapse = ", "),
            " -- expected format <conceptId><S|M><D?>, e.g. \"317009SD\""
        )
    }

    tibble::tibble(
        token = tokens,
        concept_id = vapply(matches, function(m) as.integer(m[2]), integer(1)),
        column = vapply(matches, function(m) {
            if (m[3] == "S") "concept_id" else "maps_to_concept_id"
        }, character(1)),
        expand_descendants = vapply(matches, function(m) nzchar(m[4]), logical(1))
    )
}

#' Expand concept ids to themselves and all their descendants
#'
#' @description
#' One batched `concept_ancestor` query for every concept id that needs
#' descendant expansion (never one query per id). Descendant sets include the
#' concept itself, via `concept_ancestor`'s ancestor-equals-descendant
#' self-row.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param conceptIds Integer vector of concept ids to expand.
#'
#' @return A tibble of `ancestor_concept_id`, `descendant_concept_id`.
#'
#' @importFrom DatabaseConnector renderTranslateQuerySql
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate across everything
#'
.expandDescendantConceptIds <- function(CDMdbHandler, conceptIds) {
    connection <- CDMdbHandler$connectionHandler$getConnection()
    vocabularyDatabaseSchema <- CDMdbHandler$vocabularyDatabaseSchema

    sql <- "
    SELECT DISTINCT
        ca.ancestor_concept_id AS ancestor_concept_id,
        ca.descendant_concept_id AS descendant_concept_id
    FROM @vocabularyDatabaseSchema.concept_ancestor ca
    WHERE ca.ancestor_concept_id IN (@conceptIds);
    "
    descendants <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptIds = paste(unique(conceptIds), collapse = ",")
    ) |>
        tibble::as_tibble()
    names(descendants) <- tolower(names(descendants))

    descendants |> dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))
}

#' Resolve every parsed token to its concrete id set
#'
#' @description
#' For tokens tagged with descendant expansion (`D`), resolves the concept
#' and all its descendants via a single shared `.expandDescendantConceptIds()`
#' call across all such tokens; other tokens resolve to just their own
#' concept id.
#'
#' @param CDMdbHandler A CDMdbHandler object that contains database connection details
#' @param parsedTokens Output of `.parsePersonCountsConceptIds()`.
#'
#' @return `parsedTokens` with an added list-column `resolved_ids`.
#'
#' @importFrom dplyr filter pull rowwise mutate ungroup
#' @importFrom purrr map_int
#'
.resolveTaggedConceptIdSets <- function(CDMdbHandler, parsedTokens) {
    idsNeedingExpansion <- parsedTokens |>
        dplyr::filter(expand_descendants) |>
        dplyr::pull(concept_id) |>
        unique()

    descendantsByAncestor <- if (length(idsNeedingExpansion) > 0) {
        .expandDescendantConceptIds(CDMdbHandler, idsNeedingExpansion)
    } else {
        tibble::tibble(ancestor_concept_id = integer(0), descendant_concept_id = integer(0))
    }

    resolvedTokens <- parsedTokens |>
        dplyr::rowwise() |>
        dplyr::mutate(
            resolved_ids = list(
                if (expand_descendants) {
                    descendantsByAncestor |>
                        dplyr::filter(ancestor_concept_id == concept_id) |>
                        dplyr::pull(descendant_concept_id) |>
                        unique()
                } else {
                    concept_id
                }
            )
        ) |>
        dplyr::ungroup()

    emptyTokens <- resolvedTokens$token[purrr::map_int(resolvedTokens$resolved_ids, length) == 0]
    if (length(emptyTokens) > 0) {
        stop("no concept found for token(s): ", paste(emptyTokens, collapse = ", "))
    }

    resolvedTokens
}
