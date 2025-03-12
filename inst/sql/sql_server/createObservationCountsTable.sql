-- Modified from Achilles https://github.com/OHDSI/Achilles/blob/main/inst/sql/sql_server/analyses/116.sql
DROP TABLE IF EXISTS @resultsDatabaseSchema.observation_counts;

select distinct 
  YEAR(observation_period_start_date) as obs_year 
INTO
  #temp_dates_116
from 
  @cdmDatabaseSchema.observation_period
;

--HINT DISTRIBUTE_ON_KEY(stratum_1)
WITH rawData AS (
  select
    t1.obs_year as  calendar_year,
    p1.gender_concept_id as gender_concept_id,
    floor((t1.obs_year - p1.year_of_birth)/10) as age_decile,
    COUNT_BIG(distinct p1.PERSON_ID) as count_value
  from
    @cdmDatabaseSchema.person p1
    inner join
    @cdmDatabaseSchema.observation_period op1
    on p1.person_id = op1.person_id
    ,
    #temp_dates_116 t1
  where year(op1.OBSERVATION_PERIOD_START_DATE) <= t1.obs_year
    and year(op1.OBSERVATION_PERIOD_END_DATE) >= t1.obs_year
  group by t1.obs_year,
    p1.gender_concept_id,
    floor((t1.obs_year - p1.year_of_birth)/10)
)
SELECT
  116 as analysis_id,
  CAST(calendar_year AS VARCHAR(255)) as calendar_year,
  cast(gender_concept_id as varchar(255)) as gender_concept_id,
  cast(age_decile as varchar(255)) as age_decile,
  count_value
INTO @resultsDatabaseSchema.observation_counts
FROM rawData;

TRUNCATE TABLE #temp_dates_116;
DROP TABLE #temp_dates_116;