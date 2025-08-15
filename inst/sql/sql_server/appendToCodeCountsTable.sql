
-- Insert into code_counts table
INSERT INTO @resultsDatabaseSchema.code_counts

-- calculate counts per each group of concept_id, calendar_year, gender_concept_id, age_decile
WITH code_counts AS (
    SELECT 
        ccm.concept_id AS concept_id,
        ccm.calendar_year AS calendar_year,
        ccm.gender_concept_id AS gender_concept_id,
        ccm.age_decile AS age_decile,
        COUNT_BIG(*) AS event_counts,
        COUNT_BIG(DISTINCT ccm.person_id) AS person_counts,
        COUNT(DISTINCT CASE WHEN ccm.calendar_year = ccm.min_calendar_year THEN ccm.person_id END) AS incidence_person_counts
    FROM (
        -- get all person_ids with the concept_id with in a valid observation period
        -- calculate the calendar year, gender_concept_id, age_decile
        -- calculate the min_calendar_year, used to find the first event in history  per code and person 
        SELECT 
            p.person_id AS person_id,
            t.@concept_id_field AS concept_id,
            YEAR(t.@date_field) AS calendar_year,
            p.gender_concept_id AS gender_concept_id,
            FLOOR((YEAR(t.@date_field) - p.year_of_birth) / 10) AS age_decile,
            MIN(YEAR(t.@date_field)) OVER (PARTITION BY p.person_id, t.@concept_id_field) AS min_calendar_year
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
        ccm.calendar_year,
        ccm.gender_concept_id,
        ccm.age_decile
), 
-- append descendat counts, for event counts, person counts, incidence person counts
-- for each group of concept_id, calendar_year, gender_concept_id, age_decile
descendant_counts AS (
    SELECT 
        ca.ancestor_concept_id AS concept_id,
        cctosum.calendar_year AS calendar_year,
        cctosum.gender_concept_id AS gender_concept_id,
        cctosum.age_decile AS age_decile,
        COALESCE(cc.event_counts, 0) AS event_counts,
        COALESCE(cc.person_counts, 0) AS person_counts,
        COALESCE(cc.incidence_person_counts, 0) AS incidence_person_counts,
        SUM(COALESCE(cctosum.event_counts, 0)) AS descendant_event_counts,
        SUM(COALESCE(cctosum.person_counts, 0)) AS descendant_person_counts,
        SUM(COALESCE(cctosum.incidence_person_counts, 0)) AS descendant_incidence_person_counts
    FROM
        @cdmDatabaseSchema.concept_ancestor ca
    INNER JOIN
        code_counts cctosum
    ON
       ca.descendant_concept_id = cctosum.concept_id
    LEFT JOIN
        code_counts cc
    ON
        ca.ancestor_concept_id = cc.concept_id
        AND cctosum.calendar_year = cc.calendar_year
        AND cctosum.gender_concept_id = cc.gender_concept_id
        AND cctosum.age_decile = cc.age_decile
    GROUP BY
        ca.ancestor_concept_id,
        cctosum.calendar_year,
        cctosum.gender_concept_id,
        cctosum.age_decile,
        cc.event_counts,
        cc.person_counts,
        cc.incidence_person_counts
)

-- append total person counts and save to table
SELECT 
    CAST('@domain_id' AS VARCHAR(255)) AS domain,
    CAST(ccd.concept_id AS BIGINT) AS concept_id,
    CAST(ccd.calendar_year AS INTEGER) AS calendar_year,
    CAST(ccd.gender_concept_id AS BIGINT) AS gender_concept_id,
    CAST(ccd.age_decile AS INTEGER) AS age_decile,
    CAST(ccd.event_counts AS BIGINT) AS event_counts,
    CAST(ccd.person_counts AS BIGINT) AS person_counts, 
    CAST(ccd.incidence_person_counts AS INTEGER) AS incidence_person_counts,
    CAST(ccd.descendant_event_counts AS BIGINT) AS descendant_event_counts,
    CAST(ccd.descendant_person_counts AS BIGINT) AS descendant_person_counts,
    CAST(ccd.descendant_incidence_person_counts AS INTEGER) AS descendant_incidence_person_counts,
    CAST(oc.total_person_counts AS BIGINT) AS total_person_counts
FROM
    descendant_counts ccd
JOIN
    @resultsDatabaseSchema.observation_counts oc
ON
    CAST(ccd.calendar_year AS VARCHAR(255)) = oc.calendar_year
    AND CAST(ccd.gender_concept_id AS VARCHAR(255)) = oc.gender_concept_id 
    AND CAST(ccd.age_decile AS VARCHAR(255)) = oc.age_decile
