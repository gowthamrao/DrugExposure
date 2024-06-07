#' Create Drug exposure cohort
#'
#' This function takes an input a table with person_id, drug_exposure_start_date, drug_exposure_end_date and creates
#' a cohort table in the form of subject_id, cohort_start_date, cohort_end_date
#'
#' @template Connection
#' @template ConceptSetExpression
#' @template CdmDatabaseSchema
#' @template TempEmulationSchema
#' @param restrictToFirstObservationperiod (optional) Default TRUE
#' @param conceptSetTable A temp table that hold the resolved Concept set.
#' @param outputCohortTable (optional) A temp table that holds the output. This will be created.
#' @param outputCohortId (optional) The cohort id of the output cohort. Default 0.
#' @param drugFirstStartLeftCensorDate (optional) The earliest date from which to consider drug exposures. Used to left-censor data.
#' @param drugFirstStartRightCensorDate (optional) The latest date up to which to consider drug exposures. Used to right-censor data.
getEarliestDrugExposureCohort <-
  function(connection,
           conceptSetExpression,
           cdmDatabaseSchema,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           outputCohortTable = "#de_no_subset",
           outputCohortId = 1,
           conceptSetTable = "#concept_sets",
           drugFirstStartLeftCensorDate = NULL,
           drugFirstStartRightCensorDate = NULL,
           restrictToFirstObservationperiod = TRUE) {
    
    drugFirstStartLeftCensorDate <-
      format(as.Date(ifelse(
        !is.null(drugFirstStartLeftCensorDate),
        drugFirstStartLeftCensorDate,
        NA
      )), "%Y%m%d")
    
    drugFirstStartRightCensorDate <-
      format(as.Date(ifelse(
        !is.null(drugFirstStartRightCensorDate),
        drugFirstStartRightCensorDate,
        NA
      )), "%Y%m%d")
    
    
    sql <- "

    DROP TABLE IF EXISTS @output_temp_table;

    SELECT CAST(0 AS BIGINT) as cohort_definition_id,
          de.person_id subject_id,
          de.cohort_start_date,
          op.observation_period_end_date cohort_end_date
    INTO @output_temp_table
    FROM
    (
      SELECT person_id,
              min(cohort_start_date) cohort_start_date
      FROM
      (
      	SELECT person_id,
    		      CAST(min(drug_exposure_start_date) AS DATE) cohort_start_date
      	FROM @cdm_database_schema.drug_exposure de
      	INNER JOIN @concept_set_table cs
      		ON de.drug_concept_id = cs.concept_id
        WHERE	 de.drug_concept_id > 0
        {@use_left_censor} ? {AND min_start_date > CAST('@left_censor_date' AS DATE) }
        {@use_right_censor} ? {AND min_start_date > CAST('@right_censor_date' AS DATE)}
      	GROUP BY person_id

      	UNION

      	SELECT person_id,
    		      CAST(min(drug_exposure_start_date) AS DATE) cohort_start_date
      	FROM @cdm_database_schema.drug_exposure de
      	INNER JOIN @concept_set_table cs
      		ON de.drug_source_concept_id = cs.concept_id
        WHERE	 de.drug_source_concept_id > 0
        {@use_left_censor} ? {AND min_start_date > CAST('@left_censor_date' AS DATE) }
        {@use_right_censor} ? {AND min_start_date > CAST('@right_censor_date' AS DATE)}
      	GROUP BY person_id
      ) f
      GROUP BY person_id
    ) de
    INNER JOIN
      {@restrict_first_observation_period} ? {(
                  SELECT person_id,
                          min(op.observation_period_start_date) observation_period_start_date,
                          min(op.observation_period_end_date) observation_period_end_date
                  FROM @cdm_database_schema.observation_period op
                  GROUP BY person_id
    )} : {@cdm_database_schema.observation_period} op
    ON de.person_id = op.person_id
      AND op.observation_period_start_date <= de.cohort_start_date
      AND op.observation_period_end_date >= de.cohort_start_date
    ;
  "
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      cdm_database_schema = cdmDatabaseSchema,
      use_left_censor = !is.na(drugFirstStartLeftCensorDate),
      left_censor_date = drugFirstStartLeftCensorDate,
      use_right_censor = !is.na(drugFirstStartRightCensorDate),
      right_censor_date = drugFirstStartRightCensorDate,
      restrict_first_observation_period = restrictToFirstObservationperiod,
      output_temp_table = outputCohortTable,
      concept_set_table = conceptSetTable
    )
  }