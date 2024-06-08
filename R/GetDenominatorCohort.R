#' Create denominator cohort
#'
#' This function takes as an input a concept set expression, cdmDatabaseSchema and a list of cohort subset operators. It
#' then identifies the first drug_exposure in the first (Default) or all observation_period record for the person and
#' starting from the first date of drug_expoure in the observation_period (making it cohort_start_date), defines the
#' cohort_end_date based on maxFollowUpDays bound to the same observation period. ie. observation period end date >= cohort_end_date.
#'
#' @template Connection
#' @template ConceptSetExpression
#' @template CdmDatabaseSchema
#' @template TempEmulationSchema
#' @template RestrictToFirstObservationperiod
#' @template MaxFollowUpDays
#' @template CohortGeneratorSubsetOperators
#' @template ConceptSetTable
#' @param denominatorCohortTable (optional) A temp table that holds the output. This will be created.
#' @param denominatorCohortId (optional) The cohort id of the denominator cohort. Default 0.
getDenominatorCohort <-
  function(connection,
           conceptSetExpression,
           cdmDatabaseSchema,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           denominatorCohortTable = "#denominator",
           denominatorCohortId = 1,
           conceptSetTable = "#concept_sets",
           restrictToFirstObservationperiod = TRUE,
           maxFollowUpDays = 365,
           cohortGeneratorSubsetOperators = defaultCohortGeneratorSubsetOperator()) {
    sql <- "

    DROP TABLE IF EXISTS #candidate_cohort;

    SELECT CAST(@denominator_cohort_id AS BIGINT) as cohort_definition_id,
          de.person_id subject_id,
          de.cohort_start_date,
          CASE WHEN op.observation_period_end_date >
              DATEADD(day, @max_follow_up_days, cohort_start_date) THEN
                DATEADD(day, @max_follow_up_days, cohort_start_date)
            ELSE op.observation_period_end_date
          END cohort_end_date
    INTO #candidate_cohort
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
      	GROUP BY person_id

      	UNION

      	SELECT person_id,
    		      CAST(min(drug_exposure_start_date) AS DATE) cohort_start_date
      	FROM @cdm_database_schema.drug_exposure de
      	INNER JOIN @concept_set_table cs
      		ON de.drug_source_concept_id = cs.concept_id
        WHERE	 de.drug_source_concept_id > 0
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
      restrict_first_observation_period = restrictToFirstObservationperiod,
      concept_set_table = conceptSetTable,
      denominator_cohort_id = denominatorCohortId,
      max_follow_up_days = maxFollowUpDays
    )
    
    sqlToSubset <-
      dplyr::tibble(
        cohortId = 0,
        cohortName = "Denominator",
        sql = "",
        json = ""
      ) |>
      CohortGenerator::addCohortSubsetDefinition(
        cohortSubsetDefintion = CohortGenerator::createCohortSubsetDefinition(
          name = "",
          definitionId = 1,
          subsetOperators = cohortGeneratorSubsetOperators
        ),
        targetCohortIds = c(0)
      ) |>
      dplyr::filter(.data$cohortId == 1) |>
      dplyr::pull(sql) |>
      stringr::str_replace_all(pattern = stringr::fixed("@cohort_database_schema."),
                               replacement = "")
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sqlToSubset,
      cdm_database_schema = cdmDatabaseSchema,
      cohort_table = "#candidate_cohort"
    )
    
    
    sql <- "SELECT CAST(0 AS BIGINT) cohort_definition_id,
                  subject_id,
                  cohort_start_date,
                  cohort_end_date
            INTO @cohort_table
            FROM #candidate_cohort
            WHERE cohort_definition_id = 1;

          DROP TABLE IF EXISTS #candidate_cohort;"
    
    DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                                 sql = sql,
                                                 cohort_table = denominatorCohortTable)
    
  }
