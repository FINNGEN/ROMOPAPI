DROP TABLE IF EXISTS @resultsDatabaseSchema.@codeCountsTable;

CREATE TABLE @resultsDatabaseSchema.@codeCountsTable (
  concept_id int,
  calendar_year int,
  gender_concept_id int,
  age_decile int,
  event_counts int,
  descendant_event_counts int
);

INSERT INTO @resultsDatabaseSchema.@codeCountsTable 

-- TEMP: fix all must have an ancestor to itself
WITH temp_concept_ancestor AS (
  SELECT DISTINCT * FROM (
        SELECT * FROM @cdmDatabaseSchema.concept_ancestor
        UNION ALL
        SELECT DISTINCT
            ancestor_concept_id AS ancestor_concept_id,
            ancestor_concept_id AS descendant_concept_id,
            0 AS min_levels_of_separation,
            0 AS max_levels_of_separation
        FROM
            @cdmDatabaseSchema.concept_ancestor
        UNION ALL
        SELECT DISTINCT
            descendant_concept_id AS ancestor_concept_id,
            descendant_concept_id AS descendant_concept_id,
            0 AS min_levels_of_separation,
            0 AS max_levels_of_separation
        FROM
            @cdmDatabaseSchema.concept_ancestor
    )
),
atomic_code_counts AS (
    SELECT DISTINCT
         concept_id, calendar_year, gender_concept_id, age_decile, event_counts
     FROM @resultsDatabaseSchema.@codeAtomicCountsTable
),
-- END TEMP
-- append descendat counts, for event counts, person counts, incidence person counts
-- for each group of concept_id, calendar_year, gender_concept_id, age_decile
descendant_counts AS (
    SELECT 
        ca.ancestor_concept_id AS concept_id,
        cctosum.calendar_year AS calendar_year,
        cctosum.gender_concept_id AS gender_concept_id,
        cctosum.age_decile AS age_decile,
        COALESCE(cc.event_counts, 0) AS event_counts,
        SUM(COALESCE(cctosum.event_counts, 0)) AS descendant_event_counts
    FROM
        temp_concept_ancestor ca -- TEMP: fix all must have an ancestor to itself
    INNER JOIN
        atomic_code_counts cctosum
    ON
       ca.descendant_concept_id = cctosum.concept_id
    LEFT JOIN
        atomic_code_counts cc
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
        cc.event_counts
)

-- append total person counts and save to table
SELECT 
    CAST(ccd.concept_id AS BIGINT) AS concept_id,
    CAST(ccd.calendar_year AS INTEGER) AS calendar_year,
    CAST(ccd.gender_concept_id AS BIGINT) AS gender_concept_id,
    CAST(ccd.age_decile AS INTEGER) AS age_decile,
    CAST(ccd.event_counts AS BIGINT) AS event_counts,
    CAST(ccd.descendant_event_counts AS BIGINT) AS descendant_event_counts
FROM
    descendant_counts ccd;