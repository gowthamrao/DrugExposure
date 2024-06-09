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
#' @template DenominatorCohortDatabaseSchema
#' @template DenominatorCohortTable
#' @template DenominatorCohortId
#' @template TempEmulationSchema
#' @template RestrictToFirstObservationperiod
#' @template PersistenceDays
#'
#' @export
runDrugExposure <- function(connectionDetails = NULL,
                            connection = NULL,
                            conceptSetExpression,
                            cdmDatabaseSchema,
                            denominatorCohortDatabaseSchema,
                            denominatorCohortTable,
                            denominatorCohortId,
                            vocabularyDatabaseSchema = cdmDatabaseSchema,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                            restrictToFirstObservationperiod = TRUE,
                            persistenceDays = c(0)) {
  denominatorCohortDatabaseSchemaCohortTable <-
    if (is.null(denominatorCohortDatabaseSchema)) {
      denominatorCohortTable
    } else {
      paste0(denominatorCohortDatabaseSchema,
             ".",
             denominatorCohortTable)
    }
  
  checkmate::assertIntegerish(
    x = persistenceDays,
    lower = 0,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE
  )
  
  output <- c()
  
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
  
  output$codeSets <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT c.*
            FROM #concept_sets co
            INNER JOIN @cdm_database_schema.concept c
            ON co.concept_id = c.concept_id;",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema
  ) |>
    dplyr::tibble()
  
  output$denominator <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM @cohort_table
            WHERE cohort_definition_id = @denominator_cohort_id;",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    cohort_table = denominatorCohortDatabaseSchemaCohortTable,
    denominator_cohort_id = denominatorCohortId
  ) |>
    dplyr::tibble()
  
  getDrugExposureInDenominatorCohort(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    conceptSetTable = "#concept_sets",
    denominatorCohortTable = denominatorCohortDatabaseSchemaCohortTable,
    denominatorCohortId = denominatorCohortId,
    drugExposureOutputTable = "#drug_exposure"
  )
  
  output$drugExposure <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM #drug_exposure;",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  ) |>
    dplyr::tibble()
  
  output$cohortDefinitionSet <-
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
  
  for (i in (1:nrow(output$cohortDefinitionSet))) {
    numeratorCohorts[[i]] <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM ",
        output$cohortDefinitionSet[i, ]$cohortTableName,
        ";"
      ),
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema
    ) |> dplyr::tibble()
  }
  
  output$numeratorCohorts <- dplyr::bind_rows(numeratorCohorts) |>
    dplyr::arrange(.data$cohortDefinitionId,
                   .data$subjectId)
  
  return(output)
  
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
