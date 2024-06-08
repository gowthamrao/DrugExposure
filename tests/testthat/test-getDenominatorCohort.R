testthat::test_that("Get denominator cohort", {
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

  cohort <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT min(subject_id) subject_id FROM #denominator;",
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )

  testthat::expect_true(object = nrow(cohort) >= 0)
  DatabaseConnector::disconnect(connection = connection)
})
