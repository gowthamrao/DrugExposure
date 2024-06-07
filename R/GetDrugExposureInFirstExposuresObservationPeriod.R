#' get drug exposure events for a person
#'
#' Given a concept set expression and a cohort to restrict the persons and exposure events, this function creates a temp
#' table called "#drug_exposure" that has the records that maybe used for drug exposure study.
#'
#' @template Connection
#' @template ConceptSetExpression
#' @template CdmDatabaseSchema
#' @template VocabularyDatabaseSchema
#' @template TempEmulationSchema
#' @template ConceptSetTable
#' @param drugExposureOutputTable the output table
#' @param cohortDatabaseSchema The name of the schema with cohort table. Only use when subset definitions are used.
#'
getDrugExposureInFirstExposuresObservationPeriod <-
  function(connection = NULL,
           conceptSetExpression,
           cdmDatabaseSchema,
           vocabularyDatabaseSchema = cdmDatabaseSchema,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           conceptSetTable = "#concept_sets",
           drugExposureOutputTable = "#drug_exposure_output",
           cohortDatabaseSchema = NULL) {
    
    sqlDrugExposureInFirstExposuresObservationPeriod <-
      SqlRender::readSql(
        system.file(
          "sql",
          "sql_server",
          "sqlDrugExposureInFirstExposuresObservationPeriod.sql",
          package = utils::packageName()
        )
      )
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sqlDrugExposureInFirstExposuresObservationPeriod,
      cdm_database_schema = cdmDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      drug_exposure_output = drugExposureOutputTable,
      concept_set_table = conceptSetTable
    )
    
  }