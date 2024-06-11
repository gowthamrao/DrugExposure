baseFolder <- "D://studyResults//DrugExplorer"

ROhdsiWebApi::authorizeWebApi(baseUrl = Sys.getenv("BaseUrl"), authMethod = "windows")

conceptSetExpressionOral <-
  ROhdsiWebApi::getConceptSetDefinition(conceptSetId = 7677, baseUrl = Sys.getenv("BaseUrl"))
conceptSetExpressionLai <-
  ROhdsiWebApi::getConceptSetDefinition(conceptSetId = 6496, baseUrl = Sys.getenv("BaseUrl"))

connectionDetails <-
  OhdsiHelpers::createConnectionDetails(cdmSources = cdmSources,
                                        database = "truven_mdcd",
                                        ohda = TRUE)
cdmSource <-
  OhdsiHelpers::getCdmSource(cdmSources = cdmSources, database = "truven_mdcd")
# connection <-
#   DatabaseConnector::connect(connectionDetails = connectionDetails)
# 
# resolvedConceptsOral <-
#   DatabaseConnector::renderTranslateQuerySql(
#     connection = connection,
#     sql = conceptSetExpressionOral$expression |>
#       RJSONIO::toJSON(digits = 23) |>
#       CirceR::buildConceptSetQuery(),
#     vocabulary_database_schema = cdmSource$vocabDatabaseSchema,
#     snakeCaseToCamelCase = TRUE
#   ) |>
#   dplyr::tibble()
# 
# resolvedConceptsLai <-
#   DatabaseConnector::renderTranslateQuerySql(
#     connection = connection,
#     sql = conceptSetExpressionLai$expression |>
#       RJSONIO::toJSON(digits = 23) |>
#       CirceR::buildConceptSetQuery(),
#     vocabulary_database_schema = cdmSource$vocabDatabaseSchema,
#     snakeCaseToCamelCase = TRUE
#   ) |>
#   dplyr::tibble()


output <- DrugExposure::runDrugExposure(
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

saveRDS(object = output, file = "D:\\studyResults\\epi_1177\\output.RDS")
output <- readRDS(file = "D:\\studyResults\\epi_1177\\output.RDS")

