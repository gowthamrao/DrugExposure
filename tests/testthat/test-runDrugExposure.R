testthat::test_that("Drug Exposure with connection", {
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  
  runDrugExposure(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    cdmDatabaseSchema = cdmDatabaseSchema,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    followUpDays = 365
  )
  
  codeSetId <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM #concept_sets;",
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )
  
  testthat::expect_true(nrow(codeSetId) > 0)
  
  DatabaseConnector::disconnect(connection = connection)
  
})

testthat::test_that("Drug Exposure with connection details", {
  testthat::expect_invisible(
    runDrugExposure(
      connectionDetails = connectionDetails,
      conceptSetExpression = conceptSetExpression,
      cdmDatabaseSchema = cdmDatabaseSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      followUpDays = 365,
      drugFirstStartRightCensorDate = '2025-01-01',
      drugFirstStartLeftCensorDate = '1999-01-01'
    )
  )
})
