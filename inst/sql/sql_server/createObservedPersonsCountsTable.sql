-- Population-at-risk denominator for per-year prevalence: one row per
-- calendar_year x gender_concept_id x age_decile, counting every person under
-- observation in that year (independent of any concept or visit group).
DROP TABLE IF EXISTS @resultsDatabaseSchema.@observedPersonsCountsTable;

CREATE TABLE @resultsDatabaseSchema.@observedPersonsCountsTable (
    calendar_year INTEGER,
    gender_concept_id INTEGER,
    age_decile INTEGER,
    observed_persons_counts INTEGER
);

INSERT INTO @resultsDatabaseSchema.@observedPersonsCountsTable
SELECT
    CAST(y.calendar_year AS BIGINT) AS calendar_year,
    CAST(p.gender_concept_id AS BIGINT) AS gender_concept_id,
    CAST(FLOOR((y.calendar_year - p.year_of_birth) / 10) AS BIGINT) AS age_decile,
    CAST(COUNT_BIG(DISTINCT p.person_id) AS BIGINT) AS observed_persons_counts
FROM (
    -- years bounded to what stratified_code_counts can ever show, rather than
    -- the full observation_period range (which can include far-future dates)
    SELECT DISTINCT calendar_year
    FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable
) y
CROSS JOIN @cdmDatabaseSchema.person p
INNER JOIN @cdmDatabaseSchema.observation_period op
    ON p.person_id = op.person_id
WHERE YEAR(op.observation_period_start_date) <= y.calendar_year
  AND YEAR(op.observation_period_end_date) >= y.calendar_year
GROUP BY
    y.calendar_year,
    p.gender_concept_id,
    FLOOR((y.calendar_year - p.year_of_birth) / 10);
