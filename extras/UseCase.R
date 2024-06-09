baseFolder <- "D://studyResults//DrugExplorer"

ROhdsiWebApi::authorizeWebApi(baseUrl = Sys.getenv("BaseUrl"), authMethod = "windows")

conceptSetExpression <-
  ROhdsiWebApi::getConceptSetDefinition(conceptSetId = 7677, baseUrl = Sys.getenv("BaseUrl"))

connectionDetails <-
  OhdsiHelpers::createConnectionDetails(cdmSources = cdmSources,
                                        database = "truven_mdcd",
                                        ohda = TRUE)
cdmSource <-
  OhdsiHelpers::getCdmSource(cdmSources = cdmSources, database = "truven_mdcd")

DrugExposure::runDrugExposure(
  connectionDetails = connectionDetails,
  conceptSetExpression = conceptSetExpression$expression,
  cdmDatabaseSchema = cdmSource$cdmDatabaseSchema,
  vocabularyDatabaseSchema = cdmSource$vocabDatabaseSchema,
  denominatorCohortDatabaseSchema = cdmSource$resultsDatabaseSchema,
  denominatorCohortTable = "cohort",
  denominatorCohortId = 17332,
  persistenceDays = c(0, 30, 60)
)
