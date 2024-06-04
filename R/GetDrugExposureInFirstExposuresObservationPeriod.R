#' Run Drug Exposure Analysis
#'
#' This function performs a series of SQL operations to analyze drug exposure data within a specified Common Data Model (CDM) schema. It involves creating temporal tables to capture the initial drug exposure and subsequent follow-up periods based on supplied parameters and predefined SQL logic. The function integrates SQL execution within an R environment by connecting to a database, rendering SQL templates, and executing them.
#'
#' @param connectionDetails (optional) Database connection details as required by DatabaseConnector package. Used when `connection` is NULL.
#' @param connection (optional) An existing database connection object. If NULL, a new connection will be attempted using `connectionDetails`.
#' @param conceptSetExpression Expression that defines the set of drug concepts to analyze. Usually obtained by ROhdsiWebApi from Atlas. This is converted to json and given to CirceR to get concept set expression sql.
#' @param cdmDatabaseSchema The schema name of the CDM database which contains the drug exposure table data.
#' @param vocabularyDatabaseSchema (optional) The schema name of the vocabulary database. If not provided, it defaults to `cdmDatabaseSchema`.
#' @param tempEmulationSchema (optional) The schema used for emulating temporary tables; defaults to the value set in global options.
#' @param followUpDays Number of days to follow a patient after the initial drug exposure.
#' @param drugFirstStartLeftCensorDate (optional) The earliest date from which to consider drug exposures. Used to left-censor data.
#' @param drugFirstStartRightCensorDate (optional) The latest date up to which to consider drug exposures. Used to right-censor data.
#' @param drugExposureOutputTable the output table
#'
#' @export
getDrugExposureInFirstExposuresObservationPeriod <-
  function(connectionDetails = NULL,
           connection = NULL,
           conceptSetExpression,
           cdmDatabaseSchema,
           vocabularyDatabaseSchema = cdmDatabaseSchema,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           followUpDays = 365,
           drugFirstStartLeftCensorDate = NULL,
           drugFirstStartRightCensorDate = NULL,
           drugExposureOutputTable = "#drug_exposure_output") {
    
    drugFirstStartLeftCensorDate <-
      format(as.Date(ifelse(
        !is.null(drugFirstStartLeftCensorDate),
        drugFirstStartLeftCensorDate,
        NA
      )), "%Y%m%d")
    drugFirstStartRightCensorDate <-
      format(as.Date(ifelse(
        !is.null(drugFirstStartRightCensorDate),
        drugFirstStartRightCensorDate,
        NA
      )), "%Y%m%d")
    
    checkmate::assertIntegerish(
      x = followUpDays,
      lower = 0,
      upper = 9999,
      any.missing = FALSE,
      len = 1
    )
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    createCodeSetTableFromConceptSetExpression(
      connection = connection,
      conceptSetExpression = conceptSetExpression,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      conceptSetTable = "#concept_sets"
    )
    
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
      follow_days = followUpDays,
      use_left_censor = !is.na(drugFirstStartLeftCensorDate),
      left_censor_date = drugFirstStartLeftCensorDate,
      use_right_censor = !is.na(drugFirstStartRightCensorDate),
      right_censor_date = drugFirstStartRightCensorDate,
      drug_exposure_output = drugExposureOutputTable,
      concept_set_table = "#concept_sets"
    )
    
  }