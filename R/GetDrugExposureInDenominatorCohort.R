#' get drug exposure events for a person
#'
#' Given a concept set expression and a denominator cohort to restrict the persons
#' and exposure event to period in the denominator cohort, this function creates a temp table
#' that has the records from drug_exposure table.
#'
#' @template Connection
#' @template ConceptSetExpression
#' @template QuerySource
#' @template CdmDatabaseSchema
#' @template TempEmulationSchema
#' @template ConceptSetTable
#' @template DenominatorCohortDatabaseSchema
#' @template DenominatorCohortTable
#' @template DenominatorCohortId
#' @param drugExposureOutputTable the output table
#'
getDrugExposureInDenominatorCohort <-
  function(connection = NULL,
           conceptSetExpression,
           querySource = TRUE,
           cdmDatabaseSchema,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           conceptSetTable = "#concept_sets",
           denominatorCohortDatabaseSchema = NULL,
           denominatorCohortTable = "#denominator",
           denominatorCohortId = 0,
           drugExposureOutputTable = "#drug_exposure") {
    # Validate inputs
    checkmate::assertList(conceptSetExpression, min.len = 1)
    checkmate::assertCharacter(cdmDatabaseSchema, len = 1)
    checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE)
    checkmate::assertCharacter(denominatorCohortTable, len = 1)
    checkmate::assertIntegerish(denominatorCohortId, lower = 0)
    checkmate::assertCharacter(conceptSetTable, len = 1)
    checkmate::assert_flag(querySource)

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
        paste0(
          denominatorCohortDatabaseSchema,
          ".",
          denominatorCohortTable
        )
    }

    writeLines("Creating drug exposure....")
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sqlDrugExposureInFirstExposuresObservationPeriod,
      cdm_database_schema = cdmDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      drug_exposure_output = drugExposureOutputTable,
      concept_set_table = conceptSetTable,
      denominator_cohort_table = denominatorCohortTable,
      denominator_cohort_id = denominatorCohortId,
      query_source = as.logical(querySource),
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  }
