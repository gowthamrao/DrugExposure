#' Create Numerator Cohorts
#'
#' This function takes an input a cohort (called denominator to bind the numerator cohort to), a concept set expression
#' to create the numerator cohort.
#'
#' @template Connection
#' @template CdmDatabaseSchema
#' @template TempEmulationSchema
#' @param conceptSetExpression the concept set expression, usually from ROhdsiWebApi
#' @param numeratorCohortTable Denominator cohort table.
#' @param persistenceDays Number of days to check for persistence of drug exposure.
#' @param drugExposureTable The name of the table with "#drug_exposure". Should be a temp table.
createNumeratorCohorts <- function(connection = NULL,
                                   conceptSetExpression,
                                   cdmDatabaseSchema,
                                   tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                   numeratorCohortTable = "#numerator",
                                   drugExposureTable = "#drug_exposure",
                                   persistenceDays = c(0)) {
  # sqlNonOverlappingEraWithPad <-
  #   SqlRender::readSql(
  #     system.file(
  #       "sql",
  #       "sql_server",
  #       "sqlNonOverlappingEraWithPad.sql",
  #       package = utils::packageName()
  #     )
  #   )
  # 
  # print(persistenceDays)
  # DatabaseConnector::renderTranslateExecuteSql(
  #   connection = connection,
  #   sql = sqlNonOverlappingEraWithPad,
  #   era_constructor_pad = persistenceDay,
  #   start_date = "drug_exposure_start_date",
  #   end_date = "drug_exposure_end_date",
  #   source_table = "#drug_exposure",
  #   output_table = paste0("#de_", cohortDefinitionId),
  #   cohort_definition_id = cohortDefinitionId,
  #   cohort_definition_id_not_null = !is.null(cohortDefinitionId),
  #   person_id = 'person_id',
  #   cdm_database_schema = cdmDatabaseSchema
  # )
  # 
  # cohortDefinitionSet <-
  #   dplyr::tibble(
  #     cohortId = cohortDefinitionId,
  #     cohortName = paste0("persistence ", persistenceDay),
  #     cohortTableName = paste0("#de_", cohortDefinitionId)
  #   )
  # 
  # return(cohortDefinitionSet)
  
}