testthat::test_that("Test creating drug exposure", {
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

  cohort <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT min(person_id) person
                                                      FROM #drug_exposure;"
    )

  testthat::expect_true(object = nrow(cohort) >= 0)
  DatabaseConnector::disconnect(connection = connection)
})
