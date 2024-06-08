#' Create Numerator Cohorts
#'
#' This function takes an input a cohort (called denominator to bind the numerator cohort to), a concept set expression
#' to create the numerator cohort.
#'
#' @template Connection
#' @template CdmDatabaseSchema
#' @template TempEmulationSchema
#' @param numeratorCohortTableBaseName The name of the output table
#' @param persistenceDays Number of days to check for persistence of drug exposure.
#' @param drugExposureTable The name of the table with "#drug_exposure". Should be a temp table.
#' @param baseCohortDefinitionId The minimum cohortId to create cohorts for all persistenceDays
getNumeratorCohorts <- function(connection = NULL,
                                cdmDatabaseSchema,
                                tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                numeratorCohortTableBaseName = "#numerator",
                                drugExposureTable = "#drug_exposure",
                                persistenceDays = c(0),
                                baseCohortDefinitionId = 100) {
  persistenceDays <- persistenceDays |>
    unique() |>
    sort()

  sqlNonOverlappingEraWithPad <-
    SqlRender::readSql(
      system.file(
        "sql",
        "sql_server",
        "sqlNonOverlappingEraWithPad.sql",
        package = utils::packageName()
      )
    )

  cohortDefinitionSet <- c()

  for (i in (1:length(persistenceDays))) {
    cohortDefinitionId <- baseCohortDefinitionId + i

    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sqlNonOverlappingEraWithPad,
      start_date = "drug_exposure_start_date",
      end_date = "drug_exposure_end_date",
      source_table = drugExposureTable,
      output_table = paste0(numeratorCohortTableBaseName, "_", cohortDefinitionId),
      cohort_definition_id = i + 10,
      cohort_definition_id_not_null = !is.null(cohortDefinitionId),
      person_id = "person_id",
      cdm_database_schema = cdmDatabaseSchema,
      era_constructor_pad = persistenceDays[[i]]
    )

    cohortDefinitionSet[[i]] <-
      dplyr::tibble(
        cohortId = cohortDefinitionId,
        cohortName = paste0("persistence ", persistenceDays[[i]]),
        persistenceDay = persistenceDays[[i]],
        cohortTableName = paste0(numeratorCohortTableBaseName, "_", cohortDefinitionId)
      )
  }

  cohortDefinitionSet <- dplyr::bind_rows(cohortDefinitionSet)

  return(cohortDefinitionSet)
}
