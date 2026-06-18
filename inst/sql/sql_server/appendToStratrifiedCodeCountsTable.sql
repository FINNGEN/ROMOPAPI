-- Insert into code_stratified_counts table
INSERT INTO @resultsDatabaseSchema.@stratifiedCodeCountsTable

-- calculate counts per each group of concept_id, calendar_year, gender_concept_id, age_decil, visit_group_concept_id
SELECT 
        CAST(ccm.concept_id AS BIGINT) AS concept_id,
        CAST(ccm.maps_to_concept_id AS BIGINT) AS maps_to_concept_id,
        CAST(ccm.visit_group_concept_id AS BIGINT) AS visit_group_concept_id,
        CAST(ccm.calendar_year AS BIGINT) AS calendar_year,
        CAST(ccm.gender_concept_id AS BIGINT) AS gender_concept_id,
        CAST(ccm.age_decile AS BIGINT) AS age_decile,
        COUNT_BIG(*) AS record_counts,
        CAST(0 AS BIGINT) AS persons_hll_counts
FROM (
        -- get all person_ids with the concept_id with in a valid observation period
        -- calculate the calendar year, gender_concept_id, age_decile
        -- calculate the min_calendar_year, used to find the first event in history  per code and person 
        -- if visit_source_group_concept_ids are provided, calculate the visit_group_concept_id based on the given groups, 
        --   if not on a given visit_group_concept_id keep the original visit_source_concept_id as visit_group_concept_id
        --   if not event has a visit_occurrence_id or visit_source_concept_id, assign 0 as visit_group_concept_id
        SELECT 
                p.person_id AS person_id,
                t.@concept_id_field AS concept_id,
                t.@maps_to_concept_id_field AS maps_to_concept_id,
                YEAR(t.@date_field) AS calendar_year,
                p.gender_concept_id AS gender_concept_id,
                FLOOR((YEAR(t.@date_field) - p.year_of_birth) / 10) AS age_decile,
                {@visit_group_concept_ids != 0} ? {COALESCE(vmap.visit_group_concept_id, vo.visit_source_concept_id)} : {0} AS visit_group_concept_id
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
{@visit_group_concept_ids != 0}?{
        LEFT JOIN 
                @cdmDatabaseSchema.visit_occurrence vo
        ON 
                t.visit_occurrence_id = vo.visit_occurrence_id
        LEFT JOIN (
                SELECT 
                        ca.ancestor_concept_id AS visit_group_concept_id,
                        ca.descendant_concept_id AS visit_source_concept_id
                FROM
                        @cdmDatabaseSchema.concept_ancestor ca
                WHERE
                        ca.ancestor_concept_id IN (@visit_group_concept_ids)

        ) AS vmap
        ON
                vo.visit_source_concept_id = vmap.visit_source_concept_id
}
        WHERE
                t.@concept_id_field != 0
) ccm
GROUP BY
        ccm.concept_id,
        ccm.maps_to_concept_id,
        ccm.calendar_year,
        ccm.gender_concept_id,
        ccm.age_decile,
        ccm.visit_group_concept_id;