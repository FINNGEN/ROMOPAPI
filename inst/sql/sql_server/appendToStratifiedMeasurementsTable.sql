-- Insert into stratified_measurements table
INSERT INTO @resultsDatabaseSchema.@stratifiedMeasurementsTable

-- one row per event that carries a numeric value (NOT distinct: a histogram
-- counts events, so repeated identical values must each keep their own row)
SELECT
        CAST(mv.concept_id AS BIGINT) AS concept_id,
        CAST(mv.maps_to_concept_id AS BIGINT) AS maps_to_concept_id,
        CAST(mv.visit_group_concept_id AS BIGINT) AS visit_group_concept_id,
        CAST(mv.calendar_year AS BIGINT) AS calendar_year,
        CAST(mv.gender_concept_id AS BIGINT) AS gender_concept_id,
        CAST(mv.age_decile AS BIGINT) AS age_decile,
        CAST(mv.unit_concept_id AS BIGINT) AS unit_concept_id,
        mv.value_as_number AS value_as_number
FROM (
        -- get all events with the concept_id within a valid observation period
        -- calculate the calendar year, gender_concept_id, age_decile
        -- if visit_source_group_concept_ids are provided, calculate the visit_group_concept_id based on the given groups,
        --   if not on a given visit_group_concept_id keep the original visit_source_concept_id as visit_group_concept_id
        --   if not event has a visit_occurrence_id or visit_source_concept_id, assign 0 as visit_group_concept_id
        SELECT
                t.@concept_id_field AS concept_id,
                t.@maps_to_concept_id_field AS maps_to_concept_id,
                YEAR(t.@date_field) AS calendar_year,
                p.gender_concept_id AS gender_concept_id,
                FLOOR((YEAR(t.@date_field) - p.year_of_birth) / 10) AS age_decile,
                -- a missing unit is its own partition (unitless values are still valid)
                COALESCE(t.unit_concept_id, 0) AS unit_concept_id,
                t.value_as_number AS value_as_number,
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
                -- only numeric values can be histogrammed; categorical-only
                -- measurements carry their meaning in value_as_concept_id instead
                t.value_as_number IS NOT NULL
        AND (
                -- keep events even when the standard concept is unmapped (concept_id = 0)
                -- as long as the source concept is known (e.g. NOMESCO procedure codes)
                t.@concept_id_field != 0 OR t.@maps_to_concept_id_field != 0
        )
) mv;
