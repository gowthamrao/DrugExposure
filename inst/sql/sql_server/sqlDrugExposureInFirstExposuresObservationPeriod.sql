-- This SQL retrieves all drug exposure records for each patient within 
-- the same observation period of their initial drug exposure. It ensures 
-- that subsequent drug exposures are captured only if they occur during
-- the observation same period as the first exposure.

DROP TABLE IF EXISTS #drug_exposure;

WITH first_exposure
AS (
	SELECT person_id,
		min(drug_exposure_start_date) min_start_date
	FROM @cdm_database_schema.drug_exposure de
	INNER JOIN #concept_sets cs
		ON de.drug_concept_id = cs.concept_id
	WHERE drug_concept_id > 0
	GROUP BY person_id
	),
limit_observation_period
AS (
	SELECT fr.*,
		observation_period_start_date,
		observation_period_end_date
	FROM first_exposure fr
	INNER JOIN @cdm_database_schema.observation_period op
		ON fr.person_id = op.person_id
			AND fr.min_start_date >= op.observation_period_start_date
			AND fr.min_start_date <= op.observation_period_end_date
	)
SELECT *
INTO #drug_exposure
FROM
(
  SELECT de.person_id,
  	drug_concept_id,
  	drug_source_concept_id,
  	drug_exposure_start_date,
  	CASE 
  		WHEN days_supply IS NULL
  			THEN 1
  		ELSE days_supply
  		END days_supply,
  	COALESCE(DRUG_EXPOSURE_END_DATE, 
    		          DATEADD(day, DAYS_SUPPLY, DRUG_EXPOSURE_START_DATE), 
    		          DATEADD(day, 1, DRUG_EXPOSURE_START_DATE)
    		        ) AS DRUG_EXPOSURE_END_DATE
  FROM @cdm_database_schema.drug_exposure de
  INNER JOIN #concept_sets cs
  	ON de.drug_concept_id = cs.concept_id
  INNER JOIN limit_observation_period fr
  	ON fr.person_id = de.person_id
  		AND min_start_date <= de.drug_exposure_start_date
  		AND DATEADD(day, @follow_days, min_start_date) >= de.drug_exposure_start_date
  		AND de.drug_exposure_start_date >= observation_period_start_date
  		AND de.drug_exposure_start_date <= observation_period_end_date
  WHERE drug_concept_id > 0 
    {@use_left_censor} ? {AND min_start_date > CAST('@left_censor_date' AS DATE) } 
    {@use_right_censor} ? {AND min_start_date > CAST('@right_censor_date' AS DATE)}
    
  UNION
  
  SELECT de.person_id,
  	drug_concept_id,
  	drug_source_concept_id,
  	drug_exposure_start_date,
  	CASE 
  		WHEN days_supply IS NULL
  			THEN 1
  		ELSE days_supply
  		END days_supply,
  	COALESCE(DRUG_EXPOSURE_END_DATE, 
    		          DATEADD(day, DAYS_SUPPLY, DRUG_EXPOSURE_START_DATE), 
    		          DATEADD(day, 1, DRUG_EXPOSURE_START_DATE)
    		        ) AS DRUG_EXPOSURE_END_DATE
  FROM @cdm_database_schema.drug_exposure de
  INNER JOIN #concept_sets cs
  	ON de.drug_source_concept_id = cs.concept_id
  INNER JOIN limit_observation_period fr
  	ON fr.person_id = de.person_id
  		AND min_start_date <= de.drug_exposure_start_date
  		AND DATEADD(day, @follow_days, min_start_date) >= de.drug_exposure_start_date
  		AND de.drug_exposure_start_date >= observation_period_start_date
  		AND de.drug_exposure_start_date <= observation_period_end_date
  WHERE drug_source_concept_id > 0 
    {@use_left_censor} ? {AND min_start_date > CAST('@left_censor_date' AS DATE) } 
    {@use_right_censor} ? {AND min_start_date > CAST('@right_censor_date' AS DATE)}
  ) f
;