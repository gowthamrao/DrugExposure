#' Run Drug Exposure Analysis
#'
#' This function takes as a input a Circe compatible concept set expression (as r list object that can be converted to json),
#' a denominator cohort or a set of rules to create the denominator cohort, and checks for occurrence of drug exposure events
#' in the drug_exposure table of the CDM for the conceptId in the given concept expression in the period and for the subjects in
#' the denominator cohort. It then computes a series of drug utilization metrics (adherence, persistence, utilization, patterns)
#' and reports returns a list of objects that maybe utilized in a drug exposure report.
#'
#' @template ConnectionDetails
#' @template Connection
#' @template ConceptSetExpression
#' @template CdmDatabaseSchema
#' @template VocabularyDatabaseSchema
#' @template TempEmulationSchema
#' @template RestrictToFirstObservationperiod
#' @template MaxFollowUpDays
#' @template PersistenceDays
#' @template CohortGeneratorSubsetOperators
#'
#' @export
runDrugExposure <- function(connectionDetails = NULL,
                            connection = NULL,
                            conceptSetExpression,
                            cdmDatabaseSchema,
                            vocabularyDatabaseSchema = cdmDatabaseSchema,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                            restrictToFirstObservationperiod = TRUE,
                            maxFollowUpDays = 365,
                            persistenceDays = c(0),
                            cohortGeneratorSubsetOperators = defaultCohortGeneratorSubsetOperator()) {
  checkmate::assertIntegerish(
    x = maxFollowUpDays,
    lower = 0,
    upper = 9999,
    any.missing = FALSE,
    len = 1
  )
  checkmate::assertIntegerish(
    x = persistenceDays,
    lower = 0,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE
  )
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  createCodeSetTableFromConceptSetExpression(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    conceptSetTable = "#concept_sets"
  )
  
  getDenominatorCohort(
    connection = connection,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    denominatorCohortTable = "#denominator",
    denominatorCohortId = 1,
    conceptSetTable = "#concept_sets",
    restrictToFirstObservationperiod = TRUE
  )
  
  getDrugExposureInDenominatorCohort(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    conceptSetTable = "#concept_sets",
    denominatorCohortDatabaseSchema = NULL,
    denominatorCohortTable = "#denominator",
    denominatorCohortId = 0,
    drugExposureOutputTable = "#drug_exposure"
  )
  
  cohortDefinitionSet <-
    getNumeratorCohorts(
      connection = connection,
      cdmDatabaseSchema = cdmDatabaseSchema,
      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
      numeratorCohortTableBaseName = "#numerator",
      drugExposureTable = "#drug_exposure",
      persistenceDays = persistenceDays,
      baseCohortDefinitionId = 100
    )
  
  
  # sqlCohortCreation <- paste0("
  #   DROP TABLE IF EXISTS #cohorts;
  #
  #   SELECT DISTINCT cohort_definition_id,
  #           subject_id,
  #           CAST(cohort_start_date AS DATE) cohort_start_date,
  #           CAST(cohort_end_date AS DATE) cohort_end_date
  #   INTO #cohorts
  #   FROM
  #     (
  #       SELECT * FROM #first_exposure
  #       UNION ALL
  #       SELECT * FROM #new_user
  #       UNION ALL
  #       SELECT * FROM #new_user_flwd
  #       UNION ALL
  #       SELECT * FROM #first_exposure_flwd",
  #       paste0(" UNION ALL SELECT * FROM ", persistenceCohortDefinitionset$cohortTableName, collapse = " "),
  #   "
  #     ) f;
  # ")
  #
  # DatabaseConnector::renderTranslateExecuteSql(
  #   connection = connection,
  #   sql = sqlCohortCreation,
  #   tempEmulationSchema = tempEmulationSchema
  # )
  
  # DatabaseConnector::renderTranslateExecuteSql(
  #   connection = connection,
  #   sql = paste0(
  #     "
  #       DROP TABLE IF EXISTS #first_exposure;
  #       DROP TABLE IF EXISTS #new_user;
  #       DROP TABLE IF EXISTS #new_user_flwd;
  #       DROP TABLE IF EXISTS #first_exposure_flwd;",
  #     paste0(
  #       " DROP TABLE IF EXISTS ",
  #       persistenceCohortDefinitionset$cohortTableName,
  #       collapse = "; "
  #     )
  #   ),
  #   tempEmulationSchema = tempEmulationSchema
  # )
  
  # sqlDrugExposureDaySupplyDistribution <- "
  #     with drug_exposures as
  #     (
  #       SELECT DISTINCT person_id,
  #                       drug_concept_id,
  #                       drug_exposure_start_date,
  #                       max(days_supply)
  #       FROM #drug_exposure de
  #       GROUP BY person_id,
  #               drug_concept_id,
  #               drug_exposure_start_date
  #     ),
  #     drug_sequence as
  #     (
  #       SELECT drug_concept_id,
  #               ROW_NUMBER() OVER(PARTITION BY drug_concept_id,
  #                                 ORDER BY drug_exposure_start_date) dispensation_sequence
  #       FROM drug_exposures
  #       GROUP BY drug_concept_id
  #     )
  #     SELECT dispensation_sequence,
  #           drug_concept_id
  #     FROM drug_sequence
  #     GROUP BY dispensation_sequence,
  #         drug_concept_id;
  # "
  
}
