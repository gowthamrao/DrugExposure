testthat::test_that("Test creating drug exposure using temp tables", {
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)

  createCodeSetTableFromConceptSetExpression(
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

  getDrugExposureInDenominatorCohort(
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

  cohort <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT min(person_id) person
      FROM #drug_exposure;"
    )

  testthat::expect_true(object = nrow(cohort) >= 0)

  getDrugExposureInDenominatorCohort(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    conceptSetTable = "#concept_sets",
    denominatorCohortDatabaseSchema = NULL,
    denominatorCohortTable = paste0("#", denominatorCohortTable),
    denominatorCohortId = 0,
    drugExposureOutputTable = "#drug_exposure", 
    restrictToCohortPeriod = TRUE
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "DROP TABLE IF EXISTS #drug_exposure;
           DROP TABLE IF EXISTS #concept_sets;
           DROP TABLE IF EXISTS @denominator_cohort_table;",
    denominator_cohort_table = paste0("#", denominatorCohortTable)
  )
  DatabaseConnector::disconnect(connection = connection)
})
