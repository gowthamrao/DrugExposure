testthat::test_that("Resolve Concept Set Expression", {
  
  conceptIds <- resolveConceptSetExpression(
    connection = NULL,
    connectionDetails = connectionDetails,
    conceptSetExpression = conceptSetJson |> RJSONIO::fromJSON(digits = 23),
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  )

  testthat::expect_true(object = nrow(conceptIds) > 0)

  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  conceptIdsWithConnection <-
    DrugExposure:::resolveConceptSetExpression(
      connection = connection,
      connectionDetails = connectionDetails,
      conceptSetExpression = conceptSetJson |> RJSONIO::fromJSON(digits = 23),
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
  testthat::expect_true(object = nrow(conceptIdsWithConnection) > 0)
  DatabaseConnector::disconnect(connection = connection)
})
