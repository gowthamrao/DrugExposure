testthat::test_that("Create earliest DrugExposure In First ObservationPeriod Cohort", {
  
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  
  createCodeSetTableFromConceptSetExpression(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    conceptSetTable = "#concept_sets"
  )
  
  getEarliestDrugExposureCohort(
    connection = connection,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    outputCohortTable = "#de_no_subset",
    outputCohortId = 1,
    conceptSetTable = "#concept_sets",
    drugFirstStartLeftCensorDate = NULL,
    drugFirstStartRightCensorDate = NULL,
    restrictToFirstObservationperiod = TRUE
  )
  
  cohort <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT min(subject_id) subject_id FROM #de_no_subset;",
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )
  
  testthat::expect_true(object = nrow(cohort) >= 0)
  DatabaseConnector::disconnect(connection = connection)
})
