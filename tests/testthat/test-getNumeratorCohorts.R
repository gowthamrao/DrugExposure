testthat::test_that("Test creating numerator cohorts", {
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  
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
      numeratorCohortTable = "#numerator",
      drugExposureTable = "#drug_exposure",
      persistenceDays = c(0, 100, 1000),
      baseCohortDefinitionId = 100
    )
  
  cohort <-
    DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                               sql = "SELECT min(cohort_definition_id) cohort_definition_id
                                                      FROM #numerator_101")
  
  testthat::expect_true(object = nrow(cohort) >= 0)
  
  testthat::expect_true(object = nrow(cohortDefinitionSet) > 0)
  DatabaseConnector::disconnect(connection = connection)
})
