baseFolder <- "D://studyResults//epi1177"
dir.create(baseFolder, showWarnings = FALSE, recursive = TRUE)

ROhdsiWebApi::authorizeWebApi(baseUrl = Sys.getenv("BaseUrl"), authMethod = "windows")

conceptSetExpressionOral <-
  ROhdsiWebApi::getConceptSetDefinition(conceptSetId = 7677, baseUrl = Sys.getenv("BaseUrl"))
conceptSetExpressionLai <-
  ROhdsiWebApi::getConceptSetDefinition(conceptSetId = 6496, baseUrl = Sys.getenv("BaseUrl"))


# run on MDCD
connectionDetails <-
  OhdsiHelpers::createConnectionDetails(cdmSources = cdmSources,
                                        database = "truven_mdcd",
                                        ohda = TRUE)
cdmSource <-
  OhdsiHelpers::getCdmSource(cdmSources = cdmSources, database = "truven_mdcd")

mdcd <- DrugExposure::runDrugExposure(
  connectionDetails = connectionDetails,
  conceptSetExpression = conceptSetExpressionOral$expression,
  cdmDatabaseSchema = cdmSource$cdmDatabaseSchema,
  vocabularyDatabaseSchema = cdmSource$vocabDatabaseSchema,
  denominatorCohortDatabaseSchema = cdmSource$resultsDatabaseSchema,
  denominatorCohortTable = "cohort",
  denominatorCohortId = 17332,
  gapDays = c(0, 7, 14, 30, 60, 90, 120),
  querySource = FALSE
)
saveRDS(object = mdcd, file = file.path(baseFolder, "mdcd.RDS"))


# run on CCAE
connectionDetails <-
  OhdsiHelpers::createConnectionDetails(cdmSources = cdmSources,
                                        database = "truven_ccae",
                                        ohda = TRUE)
cdmSource <-
  OhdsiHelpers::getCdmSource(cdmSources = cdmSources, database = "truven_ccae")

ccae <- DrugExposure::runDrugExposure(
  connectionDetails = connectionDetails,
  conceptSetExpression = conceptSetExpressionOral$expression,
  cdmDatabaseSchema = cdmSource$cdmDatabaseSchema,
  vocabularyDatabaseSchema = cdmSource$vocabDatabaseSchema,
  denominatorCohortDatabaseSchema = cdmSource$resultsDatabaseSchema,
  denominatorCohortTable = "cohort",
  denominatorCohortId = 17332,
  gapDays = c(0, 7, 14, 30, 60, 90, 120),
  querySource = FALSE
)
saveRDS(object = mdcd, file = file.path(baseFolder, "ccae.RDS"))

