baseFolder <- "D://studyResults//DrugExplorer"

ROhdsiWebApi::authorizeWebApi(baseUrl = Sys.getenv("BaseUrl"), authMethod = "windows")

conceptSetExpression <-
  ROhdsiWebApi::getConceptSetDefinition(conceptSetId = 7677, baseUrl = Sys.getenv("BaseUrl"))

cohortTables <-
  CohortGenerator::getCohortTableNames(cohortTable = "drug_exposure")


connectionDetails <-
  OhdsiHelpers::createConnectionDetails(cdmSources = cdmSources,
                                        database = "truven_mdcd",
                                        ohda = TRUE)
cdmSource <-
  OhdsiHelpers::getCdmSource(cdmSources = cdmSources, database = "truven_mdcd")

CohortGenerator::createCohortTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cdmSource$cohortDatabaseSchema,
  cohortTableNames = cohortTables,
  incremental = TRUE
)

CohortAlgebra::copyCohorts(connectionDetails = connectionDetails,)

debug(DrugExposure::runDrugExposure)
DrugExposure::runDrugExposure(
  connectionDetails = connectionDetails,
  conceptSetExpression = conceptSetExpression$expression,
  cdmDatabaseSchema = cdmSource$cdmDatabaseSchemaFinal,
  vocabularyDatabaseSchema = cdmSource$vocabDatabaseSchemaFinal,
  restrictToFirstObservationperiod = TRUE,
  maxFollowUpDays = 365,
  persistenceDays = c(0, 30, 60)
)
