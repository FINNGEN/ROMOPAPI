suppressPackageStartupMessages({
  library(DatabaseConnector)
  library(SqlRender)
  library(dplyr)
})

set.seed(1)
nbins <- 10L

dbPath <- tempfile(fileext = ".sqlite")
con <- connect(createConnectionDetails(dbms = "sqlite", server = dbPath))

mk <- function(cid, uid, vals) {
  tibble::tibble(
    concept_id = as.integer(cid),
    unit_concept_id = as.integer(uid),
    value_as_number = as.numeric(vals)
  )
}

# deliberately nasty: heavy outliers, a zero-MAD (all-identical) partition,
# an even-n partition (median = mean of two middles), negative values
d <- bind_rows(
  mk(1, 10, c(rnorm(200, 5, 1), 1000, -500)), # extreme outliers both tails
  mk(1, 11, rnorm(150, 100, 20)),
  mk(2, 10, rep(7, 50)),                      # MAD == 0 -> soft reset path
  mk(2, 11, c(rnorm(80, 0, 2), 50))           # even n, negatives
)

insertTable(con,
  tableName = "measurement_values", data = as.data.frame(d),
  dropTableIfExists = TRUE, createTable = TRUE, tempTable = FALSE
)

sql <- "
WITH vals AS (
  SELECT concept_id, unit_concept_id, value_as_number
  FROM @resultsDatabaseSchema.measurement_values
  WHERE value_as_number IS NOT NULL
),
ranked AS (
  SELECT concept_id, unit_concept_id, value_as_number,
         ROW_NUMBER() OVER (PARTITION BY concept_id, unit_concept_id ORDER BY value_as_number) AS rn,
         COUNT(*) OVER (PARTITION BY concept_id, unit_concept_id) AS cnt
  FROM vals
),
medians AS (
  SELECT concept_id, unit_concept_id, AVG(value_as_number) AS median_value
  FROM ranked
  WHERE rn = FLOOR((cnt + 1) / 2.0) OR rn = FLOOR((cnt + 2) / 2.0)
  GROUP BY concept_id, unit_concept_id
),
devs AS (
  SELECT v.concept_id AS concept_id, v.unit_concept_id AS unit_concept_id,
         ABS(v.value_as_number - m.median_value) AS abs_dev
  FROM vals v
  INNER JOIN medians m
     ON v.concept_id = m.concept_id AND v.unit_concept_id = m.unit_concept_id
),
ranked_devs AS (
  SELECT concept_id, unit_concept_id, abs_dev,
         ROW_NUMBER() OVER (PARTITION BY concept_id, unit_concept_id ORDER BY abs_dev) AS rn,
         COUNT(*) OVER (PARTITION BY concept_id, unit_concept_id) AS cnt
  FROM devs
),
mads AS (
  SELECT concept_id, unit_concept_id, AVG(abs_dev) AS mad_value
  FROM ranked_devs
  WHERE rn = FLOOR((cnt + 1) / 2.0) OR rn = FLOOR((cnt + 2) / 2.0)
  GROUP BY concept_id, unit_concept_id
),
breaks AS (
  SELECT m.concept_id AS concept_id, m.unit_concept_id AS unit_concept_id,
         CASE WHEN d.mad_value = 0 THEN m.median_value - 0.01
              ELSE m.median_value - 7 * d.mad_value END AS break_min,
         CASE WHEN d.mad_value = 0 THEN m.median_value + 0.01
              ELSE m.median_value + 7 * d.mad_value END AS break_max
  FROM medians m
  INNER JOIN mads d
     ON m.concept_id = d.concept_id AND m.unit_concept_id = d.unit_concept_id
),
scaled AS (
  SELECT v.concept_id AS concept_id, v.unit_concept_id AS unit_concept_id,
         b.break_min AS break_min, b.break_max AS break_max,
         @n_bins * (v.value_as_number - b.break_min) / (b.break_max - b.break_min) AS value_on_bin_scale
  FROM vals v
  INNER JOIN breaks b
     ON v.concept_id = b.concept_id AND v.unit_concept_id = b.unit_concept_id
),
raw_index AS (
  SELECT concept_id, unit_concept_id, break_min, break_max,
         CASE WHEN value_on_bin_scale = FLOOR(value_on_bin_scale)
              THEN FLOOR(value_on_bin_scale) - 1
              ELSE FLOOR(value_on_bin_scale) END AS raw_idx
  FROM scaled
)
SELECT concept_id, unit_concept_id,
       CASE WHEN raw_idx < 0 THEN -1
            WHEN raw_idx >= @n_bins THEN @n_bins
            ELSE raw_idx END AS bin_index,
       MIN(break_min) AS break_min,
       MIN(break_max) AS break_max,
       COUNT(*) AS n_events
FROM raw_index
GROUP BY concept_id, unit_concept_id,
       CASE WHEN raw_idx < 0 THEN -1
            WHEN raw_idx >= @n_bins THEN @n_bins
            ELSE raw_idx END
;"

res <- renderTranslateQuerySql(con, sql, resultsDatabaseSchema = "main", n_bins = nbins) |>
  tibble::as_tibble()
names(res) <- tolower(names(res))

# --- R reference implementation of binning_soft_mad (right-closed bins) -------
ref <- d |>
  group_by(concept_id, unit_concept_id) |>
  mutate(
    med = median(value_as_number),
    madv = median(abs(value_as_number - med)),
    bmin = if_else(madv == 0, med - 0.01, med - 7 * madv),
    bmax = if_else(madv == 0, med + 0.01, med + 7 * madv),
    scaled = nbins * (value_as_number - bmin) / (bmax - bmin),
    idx0 = floor(scaled),
    idx1 = if_else(scaled == idx0, idx0 - 1, idx0),
    bin_index = if_else(idx1 < 0, -1, if_else(idx1 >= nbins, as.numeric(nbins), idx1))
  ) |>
  ungroup() |>
  count(concept_id, unit_concept_id, bin_index, name = "n_events_ref")

cmp <- full_join(
  res |> select(concept_id, unit_concept_id, bin_index, n_events),
  ref,
  by = c("concept_id", "unit_concept_id", "bin_index")
) |>
  mutate(
    n_events = tidyr::replace_na(n_events, 0),
    n_events_ref = tidyr::replace_na(n_events_ref, 0),
    match = n_events == n_events_ref
  )

cat("\n=== SQL vs R-reference bin counts ===\n")
print(cmp, n = 100)

cat("\nTOTAL ROWS:", nrow(cmp), " ALL MATCH:", all(cmp$match), "\n")
cat("SQL total events:", sum(cmp$n_events), " R total events:", sum(cmp$n_events_ref),
    " input rows:", nrow(d), "\n")

cat("\n=== break ranges per partition (outlier robustness) ===\n")
print(res |> distinct(concept_id, unit_concept_id, break_min, break_max))

cat("\n=== underflow(-1)/overflow(", nbins, ") bins present? ===\n")
print(res |> filter(bin_index %in% c(-1L, nbins)) |> select(concept_id, unit_concept_id, bin_index, n_events))

cat("\n=== BigQuery translation (head) ===\n")
bq <- translate(render(sql, resultsDatabaseSchema = "proj.ds", n_bins = nbins), targetDialect = "bigquery")
cat(substr(bq, 1, 1800), "\n...\n")

cat("\n=== sqlite translation sanity (FLOOR/ROW_NUMBER preserved?) ===\n")
sq <- translate(render(sql, resultsDatabaseSchema = "main", n_bins = nbins), targetDialect = "sqlite")
cat("FLOOR present:", grepl("FLOOR", sq, ignore.case = TRUE),
    "| ROW_NUMBER present:", grepl("ROW_NUMBER", sq, ignore.case = TRUE), "\n")

disconnect(con)
