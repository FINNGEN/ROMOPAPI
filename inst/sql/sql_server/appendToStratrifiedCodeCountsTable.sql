-- Insert into code_stratified_counts table
INSERT INTO @resultsDatabaseSchema.@stratifiedCodeCountsTable

-- calculate counts per each group of concept_id, calendar_year, gender_concept_id, age_decil
SELECT 
        CAST(ccm.concept_id AS BIGINT) AS concept_id,
        CAST(ccm.maps_to_concept_id AS BIGINT) AS maps_to_concept_id,
        CAST(ccm.calendar_year AS BIGINT) AS calendar_year,
        CAST(ccm.gender_concept_id AS BIGINT) AS gender_concept_id,
        CAST(ccm.age_decile AS BIGINT) AS age_decile,
        COUNT_BIG(*) AS record_counts
FROM (
        -- get all person_ids with the concept_id with in a valid observation period
        -- calculate the calendar year, gender_concept_id, age_decile
        -- calculate the min_calendar_year, used to find the first event in history  per code and person 
        SELECT 
                p.person_id AS person_id,
                t.@concept_id_field AS concept_id,
                t.@maps_to_concept_id_field AS maps_to_concept_id,
                YEAR(t.@date_field) AS calendar_year,
                p.gender_concept_id AS gender_concept_id,
                FLOOR((YEAR(t.@date_field) - p.year_of_birth) / 10) AS age_decile
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
) ccm
GROUP BY
        ccm.concept_id,
        ccm.maps_to_concept_id,
        ccm.calendar_year,
        ccm.gender_concept_id,
        ccm.age_decile