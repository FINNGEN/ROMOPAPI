DROP TABLE IF EXISTS @resultsDatabaseSchema.@codeCountsTable;

CREATE TABLE @resultsDatabaseSchema.@codeCountsTable (
  concept_id int,
  record_counts int,
  descendant_record_counts int,
  number_of_descendants int,
  person_counts int,
  descendant_person_counts int
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
     -- skip unmapped events, which are counted under their source concept in the branch below
     WHERE concept_id != 0
     GROUP BY
         concept_id

    UNION ALL

    SELECT DISTINCT
         maps_to_concept_id AS concept_id,
         SUM(record_counts) AS record_counts
     FROM (
        SELECT DISTINCT
            maps_to_concept_id, visit_group_concept_id, calendar_year, gender_concept_id, age_decile, record_counts
        FROM @resultsDatabaseSchema.@stratifiedCodeCountsTable
        -- do not take if maps_to_concept_id is a standard concept
        WHERE concept_id != maps_to_concept_id
     )
     GROUP BY
         maps_to_concept_id
),

-- person-level bridge, keyed by concept (same shape as atomic_code_counts, but
-- at person grain — no SUM, kept as rows so descendant rollup can COUNT DISTINCT)
atomic_person_bridge AS (
    SELECT DISTINCT
        concept_id AS concept_id,
        person_id AS person_id
    FROM @resultsDatabaseSchema.@stratifiedPersonsTable
    -- skip unmapped events, which are counted under their source concept in the branch below
    WHERE concept_id != 0

    UNION ALL

    SELECT DISTINCT
        maps_to_concept_id AS concept_id,
        person_id AS person_id
    FROM @resultsDatabaseSchema.@stratifiedPersonsTable
    -- do not take if maps_to_concept_id is a standard concept
    WHERE concept_id != maps_to_concept_id
),

-- distinct persons per concept, own events only
atomic_person_counts AS (
    SELECT
        concept_id,
        COUNT(DISTINCT person_id) AS person_counts
    FROM atomic_person_bridge
    GROUP BY concept_id
),

-- append descendat counts, for event counts, person counts, incidence person counts
-- for each group of concept_id, calendar_year, gender_concept_id, age_decile
descendant_counts AS (
    SELECT
        ca.ancestor_concept_id AS concept_id,
        COALESCE(cc.record_counts, 0) AS record_counts,
        SUM(COALESCE(cctosum.record_counts, 0)) AS descendant_record_counts,
        COUNT(*) AS number_of_descendants
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
),

-- distinct persons across the concept and all its descendants. Cannot sum
-- atomic_person_counts across descendants — the same person can appear under
-- more than one descendant — so this counts distinct persons directly.
descendant_persons AS (
    SELECT
        ca.ancestor_concept_id AS concept_id,
        COUNT(DISTINCT b.person_id) AS descendant_person_counts
    FROM
        temp_concept_ancestor ca
    INNER JOIN
        atomic_person_bridge b
    ON
        ca.descendant_concept_id = b.concept_id
    GROUP BY
        ca.ancestor_concept_id
)

-- append person counts and save to table
SELECT
    CAST(ccd.concept_id AS BIGINT) AS concept_id,
    CAST(ccd.record_counts AS BIGINT) AS record_counts,
    CAST(ccd.descendant_record_counts AS BIGINT) AS descendant_record_counts,
    CAST(ccd.number_of_descendants AS BIGINT) AS number_of_descendants,
    CAST(COALESCE(pc.person_counts, 0) AS BIGINT) AS person_counts,
    CAST(COALESCE(dp.descendant_person_counts, 0) AS BIGINT) AS descendant_person_counts
FROM
    descendant_counts ccd
LEFT JOIN
    atomic_person_counts pc
ON
    ccd.concept_id = pc.concept_id
LEFT JOIN
    descendant_persons dp
ON
    ccd.concept_id = dp.concept_id;
