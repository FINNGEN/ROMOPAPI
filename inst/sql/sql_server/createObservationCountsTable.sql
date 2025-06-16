-- Modified from Achilles https://github.com/OHDSI/Achilles/blob/main/inst/sql/sql_server/analyses/116.sql
DROP TABLE IF EXISTS @resultsDatabaseSchema.observation_counts;

CREATE TABLE @resultsDatabaseSchema.observation_counts (
  analysis_id int,
  calendar_year varchar(255),
  gender_concept_id varchar(255),
  age_decile varchar(255),
  total_person_counts int
);

INSERT INTO @resultsDatabaseSchema.observation_counts
-- Find min and max year in observation_period
WITH RECURSIVE min_max_year AS (
  SELECT 
    MIN(YEAR(observation_period_start_date)) AS min_year,
    MAX(YEAR(observation_period_end_date)) AS max_year
  FROM @cdmDatabaseSchema.observation_period
),
years AS (
  SELECT min_year AS obs_year, max_year
  FROM min_max_year
  UNION ALL
  SELECT obs_year + 1, max_year
  FROM years
  WHERE obs_year < max_year
),
rawData AS (
  select
    t1.obs_year as  calendar_year,
    p1.gender_concept_id as gender_concept_id,
    floor((t1.obs_year - p1.year_of_birth)/10) as age_decile,
    COUNT_BIG(distinct p1.PERSON_ID) as total_person_counts
  from
    years t1
    cross join
    @cdmDatabaseSchema.person p1
    inner join
    @cdmDatabaseSchema.observation_period op1
    on p1.person_id = op1.person_id
  where YEAR(op1.OBSERVATION_PERIOD_START_DATE) <= t1.obs_year
    and YEAR(op1.OBSERVATION_PERIOD_END_DATE) >= t1.obs_year
  group by 
    t1.obs_year,
    p1.gender_concept_id,
    FLOOR((t1.obs_year - p1.year_of_birth)/10)
)
SELECT
  116 as analysis_id,
  CAST(calendar_year AS VARCHAR(255)) as calendar_year,
  cast(gender_concept_id as varchar(255)) as gender_concept_id,
  cast(age_decile as varchar(255)) as age_decile,
  total_person_counts
FROM rawData;

