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
),
binned AS (
  SELECT concept_id, unit_concept_id,
         CAST(CASE WHEN raw_idx < 0 THEN -1
                   WHEN raw_idx >= @n_bins THEN @n_bins
                   ELSE raw_idx END AS INT) AS bin_index
  FROM raw_index
)
SELECT concept_id, unit_concept_id, bin_index, COUNT(*) AS n_events
FROM binned
GROUP BY concept_id, unit_concept_id, bin_index
ORDER BY concept_id, unit_concept_id, bin_index;
