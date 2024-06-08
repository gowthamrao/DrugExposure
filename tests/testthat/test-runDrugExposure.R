testthat::test_that("Test run drug exposure", {
  
  runDrugExposure(
    connectionDetails = connectionDetails,
    conceptSetExpression = conceptSetExpression,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    restrictToFirstObservationperiod = TRUE,
    maxFollowUpDays = 365,
    persistenceDays = c(0, 100, 1000),
    cohortGeneratorSubsetOperators = defaultCohortGeneratorSubsetOperator()
  )
})
