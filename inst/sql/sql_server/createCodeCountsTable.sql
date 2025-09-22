DROP TABLE IF EXISTS @resultsDatabaseSchema.@codeCountsTable;

CREATE TABLE @resultsDatabaseSchema.@codeCountsTable (
  concept_id int,
  record_counts int,
  descendant_record_counts int
);

INSERT INTO @resultsDatabaseSchema.@codeCountsTable 


WITH
-- TEMP: fix all must have an ancestor to itself
temp_concept_ancestor AS (
  SELECT DISTINCT * FROM (
        SELECT * FROM @cdmDatabaseSchema.concept_ancestor
        UNION ALL
        SELECT DISTINCT
            concept_id AS ancestor_concept_id,
            concept_id AS descendant_concept_id,
            0 AS min_levels_of_separation,
            0 AS max_levels_of_separation
        FROM
            @cdmDatabaseSchema.concept
    )
),
-- END TEMP

-- all low level record counts
 atomic_code_counts AS (
    SELECT DISTINCT
         concept_id AS concept_id, 
         SUM(record_counts) AS record_counts
     FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable
     GROUP BY
         concept_id

    UNION ALL

    SELECT DISTINCT
         maps_to_concept_id AS concept_id, 
         SUM(record_counts) AS record_counts
     FROM (
        SELECT DISTINCT
            maps_to_concept_id, calendar_year, gender_concept_id, age_decile, record_counts
        FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable
        -- do not take if maps_to_concept_id is a standard concept
        WHERE concept_id != maps_to_concept_id
     )
     GROUP BY
         maps_to_concept_id
),

-- append descendat counts, for event counts, person counts, incidence person counts
-- for each group of concept_id, calendar_year, gender_concept_id, age_decile
descendant_counts AS (
    SELECT 
        ca.ancestor_concept_id AS concept_id,
        COALESCE(cc.record_counts, 0) AS record_counts,
        SUM(COALESCE(cctosum.record_counts, 0)) AS descendant_record_counts
    FROM
        temp_concept_ancestor ca 
    INNER JOIN
        atomic_code_counts cctosum
    ON
       ca.descendant_concept_id = cctosum.concept_id
    LEFT JOIN
        atomic_code_counts cc
    ON
        ca.ancestor_concept_id = cc.concept_id
    GROUP BY
        ca.ancestor_concept_id,
        cc.record_counts
)

-- append total person counts and save to table
SELECT 
    CAST(ccd.concept_id AS BIGINT) AS concept_id,
    CAST(ccd.record_counts AS BIGINT) AS record_counts,
    CAST(ccd.descendant_record_counts AS BIGINT) AS descendant_record_counts
FROM
    descendant_counts ccd;