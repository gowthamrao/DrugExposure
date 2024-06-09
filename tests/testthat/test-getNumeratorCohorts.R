testthat::test_that("Test creating numerator cohorts using temp tables", {
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  
  DrugExposure:::createCodeSetTableFromConceptSetExpression(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    conceptSetTable = "#concept_sets"
  )
  
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlDenominatorCohort,
    tempEmulationSchema = tempEmulationSchema,
    use_cohort_database_schema = FALSE,
    cohort_database_schema = cohortDatabaseSchema,
    cdm_database_schema = cdmDatabaseSchema,
    denominator_cohort_table = paste0("#", denominatorCohortTable)
  )
  
  DrugExposure:::getDrugExposureInDenominatorCohort(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    conceptSetTable = "#concept_sets",
    denominatorCohortDatabaseSchema = NULL,
    denominatorCohortTable = paste0("#", denominatorCohortTable),
    denominatorCohortId = 0,
    drugExposureOutputTable = "#drug_exposure"
  )

  cohortDefinitionSet <-
    DrugExposure:::getNumeratorCohorts(
      connection = connection,
      cdmDatabaseSchema = cdmDatabaseSchema,
      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
      numeratorCohortTable = "#numerator",
      drugExposureTable = "#drug_exposure",
      persistenceDays = c(0, 100, 1000),
      baseCohortDefinitionId = 100
    )

  cohort <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT min(cohort_definition_id) cohort_definition_id
             FROM #numerator_101"
    )
  testthat::expect_true(object = nrow(cohort) >= 0)
  testthat::expect_true(object = nrow(cohortDefinitionSet) > 0)
  
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "DROP TABLE IF EXISTS #numerator;
           DROP TABLE IF EXISTS #drug_exposure;
           DROP TABLE IF EXISTS #concept_sets;
           DROP TABLE IF EXISTS @denominator_cohort_table;",
    denominator_cohort_table = paste0("#", denominatorCohortTable)
  )

  DatabaseConnector::disconnect(connection = connection)
})
