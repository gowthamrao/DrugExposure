-- This SQL retrieves all drug exposure records for each patient within 
-- the same observation period of their initial drug exposure. It ensures 
-- that subsequent drug exposures are captured only if they occur during
-- the observation same period as the first exposure.

DROP TABLE IF EXISTS @drug_exposure_output;

SELECT person_id,
      drug_concept_id,
      drug_source_concept_id,
      drug_exposure_start_date,
      CAST(
          COALESCE(
            DAYS_SUPPLY, 
            DATEDIFF(day, DRUG_EXPOSURE_START_DATE, DRUG_EXPOSURE_END_DATE)
          ) AS INTEGER
        ) DAYS_SUPPLY,
    	CAST(COALESCE(DRUG_EXPOSURE_END_DATE, 
      		          DATEADD(day, DAYS_SUPPLY, DRUG_EXPOSURE_START_DATE), 
      		          DATEADD(day, 1, DRUG_EXPOSURE_START_DATE)
      		        ) AS DATE) AS DRUG_EXPOSURE_END_DATE
INTO @drug_exposure_output
FROM
(
  SELECT drug_exposure_id,
          person_id,
          drug_exposure_start_date,
          drug_exposure_end_date,
          drug_concept_id,
          drug_source_concept_id,
          days_supply
  FROM @cdm_database_schema.drug_exposure de
  INNER JOIN @concept_set_table cs
  	ON de.drug_concept_id = cs.concept_id
  INNER JOIN @denominator_cohort_table dc
  	ON dc.subject_id = de.person_id
  		AND dc.cohort_start_date <= de.drug_exposure_start_date
  		AND dc.cohort_end_date >= de.drug_exposure_start_date
  WHERE drug_concept_id > 0 
    
  UNION
  
  SELECT drug_exposure_id,
          person_id,
          drug_exposure_start_date,
          drug_exposure_end_date,
          drug_concept_id,
          drug_source_concept_id,
          days_supply
  FROM @cdm_database_schema.drug_exposure de
  INNER JOIN @concept_set_table cs
  	ON de.drug_source_concept_id = cs.concept_id
  INNER JOIN @denominator_cohort_table dc
  	ON dc.subject_id = de.person_id
  		AND dc.cohort_start_date <= de.drug_exposure_start_date
  		AND dc.cohort_end_date >= de.drug_exposure_start_date
  WHERE drug_source_concept_id > 0 
  ) f
  ORDER BY person_id,
      drug_exposure_start_date
;

