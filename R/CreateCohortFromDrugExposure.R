#' Create Drug exposure cohort
#'
#' This function takes an input a table with person_id, drug_exposure_start_date, drug_exposure_end_date and creates
#' a cohort table in the form of subject_id, cohort_start_date, cohort_end_date
#'
#' @param connection (optional) An existing database connection object. If NULL, a new connection will be attempted using `connectionDetails`.
#' @param cdmDatabaseSchema The schema name of the CDM database which contains the drug exposure table data.
#' @param tempEmulationSchema (optional) The schema used for emulating temporary tables; defaults to the value set in global options.
#' @param persistenceDay Number of days to check for persistence of drug exposure.
#' @param drugExposureTable drug exposure table
#' @param conceptSetExpression the concept set expression, usually from ROhdsiWebApi
#' @param cohortDefinitionId what is the id you would like the final cohort to be?
createCohortFromDrugExposure <- function(connection = NULL,
                                         conceptSetExpression,
                                         cdmDatabaseSchema,
                                         tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                         drugExposureTable = "#drug_exposure",
                                         persistenceDay,
                                         cohortDefinitionId = NULL) {
  sqlNonOverlappingEraWithPad <-
    SqlRender::readSql(
      system.file(
        "sql",
        "sql_server",
        "sqlNonOverlappingEraWithPad.sql",
        package = utils::packageName()
      )
    )
  
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlNonOverlappingEraWithPad,
    era_constructor_pad = persistenceDay,
    start_date = "drug_exposure_start_date",
    end_date = "drug_exposure_end_date",
    source_table = "#drug_exposure",
    output_table = paste0("#de_", cohortDefinitionId),
    cohort_definition_id = cohortDefinitionId,
    cohort_definition_id_not_null = !is.null(cohortDefinitionId),
    person_id = 'person_id',
    cdm_database_schema = cdmDatabaseSchema
  )
  
  cohortDefinitionSet <-
    dplyr::tibble(
      cohortId = cohortDefinitionId,
      cohortName = paste0("persistence ", persistenceDay),
      cohortTableName = paste0("#de_", cohortDefinitionId)
    )
  
  return(cohortDefinitionSet)
  
}