#' get drug exposure events for a person
#'
#' Given a concept set expression and a denominator cohort to restrict the persons
#' and exposure event to period in the cohort, this function creates a temp table
#' that has the records from drug_exposure table.
#'
#' @template Connection
#' @template ConceptSetExpression
#' @template CdmDatabaseSchema
#' @template TempEmulationSchema
#' @template ConceptSetTable
#' @param denominatorCohortDatabaseSchema (optional) The cohort database schema 
#' that has the denominator cohort.
#' @param denominatorCohortTable Denominator cohort table.
#' @param denominatorCohortId (optional) The cohort id of the denominator cohort. Default 0.
#' @param drugExposureOutputTable the output table
#'
getDrugExposureInDenominatorCohort <-
  function(connection = NULL,
           conceptSetExpression,
           cdmDatabaseSchema,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           conceptSetTable = "#concept_sets",
           denominatorCohortDatabaseSchema = NULL,
           denominatorCohortTable = "#denominator",
           denominatorCohortId = 0,
           drugExposureOutputTable = "#drug_exposure") {
    
    sqlDrugExposureInFirstExposuresObservationPeriod <-
      SqlRender::readSql(
        system.file(
          "sql",
          "sql_server",
          "sqlDrugExposureInDenominatorCohort.sql",
          package = utils::packageName()
        )
      )
    
    if (!is.null(denominatorCohortDatabaseSchema)) {
      denominatorCohortTable <-
        paste0(denominatorCohortDatabaseSchema,
               ".",
               denominatorCohortTable)
    }
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sqlDrugExposureInFirstExposuresObservationPeriod,
      cdm_database_schema = cdmDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      drug_exposure_output = drugExposureOutputTable,
      concept_set_table = conceptSetTable,
      denominator_cohort_table = denominatorCohortTable
    )
    
  }