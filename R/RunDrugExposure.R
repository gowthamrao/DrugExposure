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
#' @param persistenceDays (optional) Number of days to check for persistence of drug exposure. Can take a array of days, and report will be generated for all.
#' @param drugFirstStartLeftCensorDate (optional) The earliest date from which to consider drug exposures. Used to left-censor data.
#' @param drugFirstStartRightCensorDate (optional) The latest date up to which to consider drug exposures. Used to right-censor data.
#'
#' @export
runDrugExposure <- function(connectionDetails = NULL,
                            connection = NULL,
                            conceptSetExpression,
                            cdmDatabaseSchema,
                            vocabularyDatabaseSchema = cdmDatabaseSchema,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                            followUpDays = 365,
                            persistenceDays = c(0),
                            drugFirstStartLeftCensorDate = NULL,
                            drugFirstStartRightCensorDate = NULL) {
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
  checkmate::assertIntegerish(
    x = persistenceDays,
    lower = 0,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE
  )
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  createCodeSetTableFromConceptSetExpression(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema
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
    right_censor_date = drugFirstStartRightCensorDate
  )
  
  sqlCreateContinuousTreatmentErasBasedOnPersistenceParameter <-
    SqlRender::readSql(
      system.file(
        "sql",
        "sql_server",
        "sqlCreateContinuousTreatmentErasBasedOnPersistenceParameter.sql",
        package = utils::packageName()
      )
    )
  
  sqlExposureCohort <- "DROP TABLE IF EXISTS #persist_0;
                        WITH first_record as
                        (
                            SELECT person_id,
                                    min(drug_exposure_start_date) min_start_date
                            FROM #drug_exposure
                            GROUP BY person_id
                        )
                        SELECT CAST(10 AS BIGINT) cohort_definition_id,
                            de.person_id subject_id,
                            drug_exposure_start_date cohort_start_date,
                            CAST(MAX(CASE WHEN drug_exposure_end_date > DATEADD(day, @follow_days, min_start_date) THEN
                                  DATEADD(day, @follow_days, min_start_date)
                                ELSE drug_exposure_end_date
                                END) AS DATE) cohort_end_date
                        INTO #persist_0
                        FROM @cdm_database_schema.drug_exposure de
                        INNER JOIN first_record fr
                        ON fr.person_id = de.person_id
                        WHERE drug_exposure_start_date <= DATEADD(day, @follow_days, min_start_date)
                        GROUP BY de.person_id,
                                  de.drug_exposure_start_date;"
  
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlExposureCohort,
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    follow_days = followUpDays
  )
  
  sqlDrugExposureStartToEndOfFollowUp <- "
        DROP TABLE IF EXISTS #denominator_cohort;
        SELECT  CAST(0 AS BIGINT) cohort_definition_id,
                p.person_id subject_id,
                p.cohort_start_date,
                CAST(CASE WHEN observation_period_end_date < cohort_end_date THEN
                    observation_period_end_date
                  ELSE cohort_end_date
                END AS DATE) cohort_end_date
        INTO #denominator_cohort
        FROM
            (
              SELECT person_id,
                      min(drug_exposure_start_date) cohort_start_date,
                      CAST(DATEADD(day, @follow_up_days, min(drug_exposure_start_date)) AS DATE) cohort_end_date
              FROM #drug_exposure
              GROUP BY person_id
            ) p
        INNER JOIN @cdm_database_schema.observation_period op
        ON p.person_id = op.person_id
          AND p.cohort_start_date >= op.observation_period_start_date
          AND p.cohort_end_date <= op.observation_period_end_date;
      "
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlDrugExposureStartToEndOfFollowUp,
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    follow_up_days = followUpDays
  )
  
  sqlFirstDrugExposureCohort <- "
        DROP TABLE IF EXISTS #first_exposure;
        SELECT  CAST(1 AS BIGINT) cohort_definition_id,
                person_id subject_id,
                drug_exposure_start_date cohort_start_date,
                CAST(max(drug_exposure_end_date) AS DATE) cohort_end_date
        INTO #first_exposure
        FROM
        (
          SELECT de.*
          FROM #drug_exposure de
          INNER JOIN
            (
              SELECT person_id,
                  min(drug_exposure_start_date) min_start
              FROM #drug_exposure
              GROUP BY person_id
            ) fe
          ON de.person_id = fe.person_id
            AND de.drug_exposure_start_date = min_start
        ) fe2
        GROUP BY person_id,
                drug_exposure_start_date;
    "
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlFirstDrugExposureCohort,
    tempEmulationSchema = tempEmulationSchema
  )
  
  sqlNewUser <- "
        DROP TABLE IF EXISTS #new_user;
        SELECT CAST(2 AS BIGINT) cohort_definition_id,
                subject_id,
                cohort_start_date,
                cohort_end_date
        INTO #new_user
        FROM #first_exposure fe
        INNER JOIN @cdm_database_schema.observation_period op
        ON fe.subject_id = op.person_id
          AND op.observation_period_start_date <= DATEADD(day, -365, fe.cohort_start_date)
          AND op.observation_period_end_date > fe.cohort_start_date;
    "
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlNewUser,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema
  )
  
  sqlObservedDuringFollowUp <- "
        DROP TABLE IF EXISTS #new_user_flwd;
        SELECT CAST(3 AS BIGINT) cohort_definition_id,
            subject_id,
            cohort_start_date,
            cohort_end_date
        INTO #new_user_flwd
        FROM #new_user fe
        INNER JOIN @cdm_database_schema.observation_period op
        ON fe.subject_id = op.person_id
          AND op.observation_period_start_date <= fe.cohort_start_date
          AND op.observation_period_end_date >= DATEADD(day, @follow_up_days, fe.cohort_start_date);


        DROP TABLE IF EXISTS #first_exposure_flwd;
        SELECT CAST(4 AS BIGINT) cohort_definition_id,
            subject_id,
            cohort_start_date,
            cohort_end_date
        INTO #first_exposure_flwd
        FROM #first_exposure fe
        INNER JOIN @cdm_database_schema.observation_period op
        ON fe.subject_id = op.person_id
          AND op.observation_period_start_date <= fe.cohort_start_date
          AND op.observation_period_end_date >= DATEADD(day, @follow_up_days, fe.cohort_start_date);
    "
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlObservedDuringFollowUp,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema,
    follow_up_days = followUpDays
  )
  
  if (!is.null(persistenceDays)) {
    # browser()
  }
  
  sqlCohortCreation <- "
    DROP TABLE IF EXISTS #cohort_drug_expd1;
    DROP TABLE IF EXISTS #cohort_drug_expd2;

    SELECT cohort_definition_id,
            subject_id,
            CAST(cohort_start_date AS DATE) cohort_start_date,
            CAST(cohort_end_date AS DATE) cohort_end_date
    INTO #cohort_drug_expd1
    FROM
      (
        SELECT * FROM #persist_0
        UNION ALL
        SELECT * FROM #denominator_cohort
        UNION ALL
        SELECT * FROM #first_exposure
        UNION ALL
        SELECT * FROM #new_user
        UNION ALL
        SELECT * FROM #new_user_flwd
        UNION ALL
        SELECT * FROM #first_exposure_flwd
      ) f;
  "
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlCohortCreation,
    tempEmulationSchema = tempEmulationSchema
  )
  
  CohortAlgebra::unionCohorts(
    connection = connection,
    sourceCohortTable = "#cohort_drug_expd1",
    targetCohortTable = "#cohort_drug_expd2",
    oldToNewCohortId = dplyr::tibble(oldCohortId = c(1:6), newCohortId = c(1:6)),
    tempEmulationSchema = tempEmulationSchema,
    isTempTable = TRUE,
    purgeConflicts = TRUE
  )
  
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "
        DROP TABLE IF EXISTS #persist_0;
        DROP TABLE IF EXISTS #denominator_cohort;
        DROP TABLE IF EXISTS #first_exposure;
        DROP TABLE IF EXISTS #new_user;
        DROP TABLE IF EXISTS #new_user_flwd;
        DROP TABLE IF EXISTS #first_exposure_flwd;",
    tempEmulationSchema = tempEmulationSchema
  )
  
  sqlDrugExposureDaySupplyDistribution <- "
      with drug_exposures as
      (
        SELECT DISTINCT person_id,
                        drug_concept_id,
                        drug_exposure_start_date,
                        max(days_supply)
        FROM #drug_exposure de
        GROUP BY person_id,
                drug_concept_id,
                drug_exposure_start_date
      ),
      drug_sequence as
      (
        SELECT drug_concept_id,
                ROW_NUMBER() OVER(PARTITION BY drug_concept_id,
                                  ORDER BY drug_exposure_start_date) dispensation_sequence
        FROM drug_exposures
        GROUP BY drug_concept_id
      )
      SELECT dispensation_sequence,
            drug_concept_id
      FROM drug_sequence
      GROUP BY dispensation_sequence,
          drug_concept_id;
  "
  
}