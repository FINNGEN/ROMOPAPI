-- Parameterized query to count codes from any domain table
INSERT INTO @resultsDatabaseSchema.code_counts
SELECT 
    CAST(cc.domain AS VARCHAR(255)) AS domain,
    CAST(cc.concept_id AS BIGINT) AS concept_id,
    CAST(cc.calendar_year AS INTEGER) AS calendar_year,
    CAST(cc.gender_concept_id AS BIGINT) AS gender_concept_id,
    CAST(cc.age_decile AS INTEGER) AS age_decile,
    CAST(cc.n_persons_with_code AS BIGINT) AS n_persons_with_code,
    CAST(oc.count_value AS BIGINT) AS n_persons_with_observation
FROM (
    SELECT 
        '@domain_id' AS domain,
        t.@concept_id_field AS concept_id,
        YEAR(t.@date_field) AS calendar_year,
        p.gender_concept_id AS gender_concept_id,
        FLOOR((YEAR(t.@date_field) - p.year_of_birth) / 10) AS age_decile,
        COUNT_BIG(DISTINCT p.person_id) AS n_persons_with_code
    FROM 
        @cdmDatabaseSchema.person p
    JOIN 
        @cdmDatabaseSchema.@table_name t
    ON 
        p.person_id = t.person_id
    JOIN 
        @cdmDatabaseSchema.observation_period op 
    ON 
        t.person_id = op.person_id
    AND 
        t.@date_field >= op.observation_period_start_date
    AND 
        t.@date_field <= op.observation_period_end_date
    WHERE
        t.@concept_id_field != 0
    GROUP BY
        t.@concept_id_field,
        YEAR(t.@date_field),
        p.gender_concept_id,
        FLOOR((YEAR(t.@date_field) - p.year_of_birth) / 10)
) cc
JOIN
    @resultsDatabaseSchema.observation_counts oc
ON
    CAST(cc.calendar_year AS VARCHAR(255)) = oc.calendar_year
    AND CAST(cc.gender_concept_id AS VARCHAR(255)) = oc.gender_concept_id 
    AND CAST(cc.age_decile AS VARCHAR(255)) = oc.age_decile