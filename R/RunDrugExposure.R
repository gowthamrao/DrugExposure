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
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  writeLines("Running SQL...")
  createCodeSetTableFromConceptSetExpression(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    conceptSetTable = "#concept_sets"
  )
  
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
  
  output <- c()
  
  output$cohortDefinitionSet <-
    getNumeratorCohorts(
      connection = connection,
      cdmDatabaseSchema = cdmDatabaseSchema,
      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
      numeratorCohortTableBaseName = "#numerator",
      drugExposureTable = "#drug_exposure",
      persistenceDays = persistenceDays,
      baseCohortDefinitionId = 100
    ) |> 
    dplyr::tibble()
  
  writeLines("Downloading....")
  output$person <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "
            SELECT person_id,
                    gender_concept_id,
                    race_concept_id,
                    ethnicity_concept_id,
                    year_of_birth
            FROM @cdm_database_schema.person p
            INNER JOIN (
                        SELECT DISTINCT subject_id
                        FROM @denominator_cohort_table
                      ) d
            ON p.person_id = d.subject_id;",
    cdm_database_schema = cdmDatabaseSchema,
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    denominator_cohort_table = denominatorCohortDatabaseSchemaCohortTable
  ) |>
    dplyr::tibble()
  
  output$codeSets <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT c.*
            FROM (
                    SELECT DISTINCT concept_id
                    FROM
                    (
                      SELECT concept_id
                      FROM #concept_sets
                      UNION ALL
                      SELECT drug_concept_id
                      FROM #drug_exposure
                      UNION ALL
                      SELECT drug_source_concept_id
                      FROM #drug_exposure
                      UNION ALL
                      SELECT DISTINCT gender_concept_id
                      FROM @cdm_database_schema.person
                      UNION ALL
                      SELECT DISTINCT race_concept_id
                      FROM @cdm_database_schema.person
                      UNION ALL
                      SELECT DISTINCT ethnicity_concept_id
                      FROM @cdm_database_schema.person
                    ) combined_concepts
                  ) co
            INNER JOIN @cdm_database_schema.concept c
            ON co.concept_id = c.concept_id;",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema
  ) |>
    dplyr::tibble()
  
  output$datesObservered <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = " SELECT  c.cohort_start_date,
                    COUNT(DISTINCT o.person_id) AS num_people
            FROM @denominator_cohort_table c
            JOIN @cdm_database_schema.observation_period o
            ON c.subject_id = o.person_id
            WHERE o.observation_period_start_date <= c.cohort_start_date AND
                  o.observation_period_end_date >= c.cohort_start_date
            GROUP BY c.cohort_start_date
            ORDER BY c.cohort_start_date;
    ",
      denominator_cohort_table = denominatorCohortDatabaseSchemaCohortTable,
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
  
  # output$drugExposure <- DatabaseConnector::renderTranslateQuerySql(
  #   connection = connection,
  #   sql = "SELECT * FROM #drug_exposure;",
  #   snakeCaseToCamelCase = TRUE,
  #   tempEmulationSchema = tempEmulationSchema
  # ) |>
  #   dplyr::tibble()
  
  numeratorCohorts <- c()
  
  for (i in (1:nrow(output$cohortDefinitionSet))) {
    numeratorCohorts[[i]] <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM ",
        output$cohortDefinitionSet[i,]$cohortTableName,
        ";"
      ),
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema
    ) |> dplyr::tibble()
  }
  
  output$numeratorCohorts <- dplyr::bind_rows(numeratorCohorts) |>
    dplyr::arrange(.data$cohortDefinitionId,
                   .data$subjectId)
  
  browser()
  
  output$denominatorTsibble<- output$denominator |> 
    dplyr::select(cohortStartDate) |> 
    dplyr::mutate(cohortStartDate = as.Date(cohortStartDate)) |> 
    tsibble::as_tibble(index = cohortStartDate, key = NULL) |> 
    tsibble::index_by(week = ~ floor_date(.index, "week")) 
  
  # to do: denominator cohort ()
  
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
