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

  codeSets <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT CONCEPT_ID FROM #concept_sets;",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  ) |>
    dplyr::tibble()

  getDenominatorCohort(
    connection = connection,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    denominatorCohortTable = "#denominator",
    denominatorCohortId = 1,
    conceptSetTable = "#concept_sets",
    restrictToFirstObservationperiod = TRUE
  )

  denominator <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM #denominator
            WHERE cohort_definition_id = 1;",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  ) |>
    dplyr::tibble()

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

  drugExposure <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM #drug_exposure;",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  ) |>
    dplyr::tibble()

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

  numeratorCohorts <- c()

  for (i in (1:nrow(cohortDefinitionSet))) {
    numeratorCohorts[[i]] <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM ",
        cohortDefinitionSet[i, ]$cohortTableName,
        ";"
      ),
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema
    ) |> dplyr::tibble()
  }

  numeratorCohorts <- dplyr::bind_rows(numeratorCohorts) |>
    dplyr::arrange(
      cohortId,
      subjectId
    )



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
