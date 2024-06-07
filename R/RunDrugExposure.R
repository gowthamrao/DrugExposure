#' #' Run Drug Exposure Analysis
#' #'
#' #' This function performs a series of SQL operations to analyze drug exposure data within a specified Common Data Model (CDM) schema. It involves creating temporal tables to capture the initial drug exposure and subsequent follow-up periods based on supplied parameters and predefined SQL logic. The function integrates SQL execution within an R environment by connecting to a database, rendering SQL templates, and executing them.
#' #'
#' #' @param connectionDetails (optional) Database connection details as required by DatabaseConnector package. Used when `connection` is NULL.
#' #' @param connection (optional) An existing database connection object. If NULL, a new connection will be attempted using `connectionDetails`.
#' #' @param conceptSetExpression Expression that defines the set of drug concepts to analyze. Usually obtained by ROhdsiWebApi from Atlas. This is converted to json and given to CirceR to get concept set expression sql.
#' #' @param cdmDatabaseSchema The schema name of the CDM database which contains the drug exposure table data.
#' #' @param vocabularyDatabaseSchema (optional) The schema name of the vocabulary database. If not provided, it defaults to `cdmDatabaseSchema`.
#' #' @param tempEmulationSchema (optional) The schema used for emulating temporary tables; defaults to the value set in global options.
#' #' @param followUpDays Number of days to follow a patient after the initial drug exposure.
#' #' @param persistenceDays (optional) Number of days to check for persistence of drug exposure. Can take a array of days, and report will be generated for all.
#' #' @param drugFirstStartLeftCensorDate (optional) The earliest date from which to consider drug exposures. Used to left-censor data.
#' #' @param drugFirstStartRightCensorDate (optional) The latest date up to which to consider drug exposures. Used to right-censor data.
#' #'
#' #' @export
#' runDrugExposure <- function(connectionDetails = NULL,
#'                             connection = NULL,
#'                             conceptSetExpression,
#'                             cdmDatabaseSchema,
#'                             vocabularyDatabaseSchema = cdmDatabaseSchema,
#'                             tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
#'                             followUpDays = 365,
#'                             persistenceDays = c(0),
#'                             drugFirstStartLeftCensorDate = NULL,
#'                             drugFirstStartRightCensorDate = NULL) {
#'   checkmate::assertIntegerish(
#'     x = followUpDays,
#'     lower = 0,
#'     upper = 9999,
#'     any.missing = FALSE,
#'     len = 1
#'   )
#'   checkmate::assertIntegerish(
#'     x = persistenceDays,
#'     lower = 0,
#'     any.missing = FALSE,
#'     min.len = 1,
#'     unique = TRUE
#'   )
#'   
#'   if (is.null(connection)) {
#'     connection <- DatabaseConnector::connect(connectionDetails)
#'     on.exit(DatabaseConnector::disconnect(connection))
#'   }
#'   
#'   getDrugExposureInFirstExposuresObservationPeriod(
#'     connection = connection,
#'     conceptSetExpression = conceptSetExpression,
#'     cdmDatabaseSchema = cdmDatabaseSchema,
#'     vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#'     tempEmulationSchema = tempEmulationSchema,
#'     followUpDays = followUpDays,
#'     drugFirstStartLeftCensorDate = drugFirstStartLeftCensorDate,
#'     drugFirstStartRightCensorDate = drugFirstStartRightCensorDate,
#'     drugExposureOutputTable = "#drug_exposure"
#'   )
#'   
#'   persistenceDays <- c(0, 9999, followUpDays, persistenceDays) |> sort() |> unique()
#'   
#'   persistenceCohortDefinitionset <- c()
#'   for (i in (1:length(persistenceDays))) {
#'     
#'     persistenceDay <- persistenceDays[[i]]
#'     
#'     persistenceCohortDefinitionset[[i]] <-
#'       createCohortFromConceptSetExpression(
#'         connection = connection,
#'         cdmDatabaseSchema = cdmDatabaseSchema,
#'         tempEmulationSchema = tempEmulationSchema,
#'         drugExposureTable = "#drug_exposure",
#'         persistenceDay = persistenceDay,
#'         cohortDefinitionId = 10 + i
#'       )
#'   }
#'   
#'   persistenceCohortDefinitionset <- dplyr::bind_rows(persistenceCohortDefinitionset)
#'   
#'   sqlFirstDrugExposureCohort <- "
#'         DROP TABLE IF EXISTS #first_exposure;
#'         SELECT  CAST(1 AS BIGINT) cohort_definition_id,
#'                 subject_id,
#'                 CAST(min(cohort_start_date) AS DATE) cohort_start_date,
#'                 CAST(min(cohort_end_date) AS DATE)  cohort_end_date
#'         INTO #first_exposure
#'         FROM #de_11
#'         GROUP BY subject_id;
#'     "
#'   DatabaseConnector::renderTranslateExecuteSql(
#'     connection = connection,
#'     sql = sqlFirstDrugExposureCohort,
#'     tempEmulationSchema = tempEmulationSchema
#'   )
#'   
#'   sqlNewUser <- "
#'         DROP TABLE IF EXISTS #new_user;
#'         SELECT CAST(2 AS BIGINT) cohort_definition_id,
#'                 subject_id,
#'                 cohort_start_date,
#'                 cohort_end_date
#'         INTO #new_user
#'         FROM #first_exposure fe
#'         INNER JOIN @cdm_database_schema.observation_period op
#'         ON fe.subject_id = op.person_id
#'           AND op.observation_period_start_date <= DATEADD(day, -365, fe.cohort_start_date)
#'           AND op.observation_period_end_date > fe.cohort_start_date;
#'     "
#'   DatabaseConnector::renderTranslateExecuteSql(
#'     connection = connection,
#'     sql = sqlNewUser,
#'     tempEmulationSchema = tempEmulationSchema,
#'     cdm_database_schema = cdmDatabaseSchema
#'   )
#'   
#'   sqlObservedDuringFollowUp <- "
#'         DROP TABLE IF EXISTS #new_user_flwd;
#'         SELECT CAST(3 AS BIGINT) cohort_definition_id,
#'             subject_id,
#'             cohort_start_date,
#'             cohort_end_date
#'         INTO #new_user_flwd
#'         FROM #new_user fe
#'         INNER JOIN @cdm_database_schema.observation_period op
#'         ON fe.subject_id = op.person_id
#'           AND op.observation_period_start_date <= fe.cohort_start_date
#'           AND op.observation_period_end_date >= DATEADD(day, @follow_up_days, fe.cohort_start_date);
#' 
#' 
#'         DROP TABLE IF EXISTS #first_exposure_flwd;
#'         SELECT CAST(4 AS BIGINT) cohort_definition_id,
#'             subject_id,
#'             cohort_start_date,
#'             cohort_end_date
#'         INTO #first_exposure_flwd
#'         FROM #first_exposure fe
#'         INNER JOIN @cdm_database_schema.observation_period op
#'         ON fe.subject_id = op.person_id
#'           AND op.observation_period_start_date <= fe.cohort_start_date
#'           AND op.observation_period_end_date >= DATEADD(day, @follow_up_days, fe.cohort_start_date);
#'     "
#'   DatabaseConnector::renderTranslateExecuteSql(
#'     connection = connection,
#'     sql = sqlObservedDuringFollowUp,
#'     tempEmulationSchema = tempEmulationSchema,
#'     cdm_database_schema = cdmDatabaseSchema,
#'     follow_up_days = followUpDays
#'   )
#'   
#' 
#'   sqlCohortCreation <- paste0("
#'     DROP TABLE IF EXISTS #cohorts;
#' 
#'     SELECT DISTINCT cohort_definition_id,
#'             subject_id,
#'             CAST(cohort_start_date AS DATE) cohort_start_date,
#'             CAST(cohort_end_date AS DATE) cohort_end_date
#'     INTO #cohorts
#'     FROM
#'       (
#'         SELECT * FROM #first_exposure
#'         UNION ALL
#'         SELECT * FROM #new_user
#'         UNION ALL
#'         SELECT * FROM #new_user_flwd
#'         UNION ALL
#'         SELECT * FROM #first_exposure_flwd",
#'         paste0(" UNION ALL SELECT * FROM ", persistenceCohortDefinitionset$cohortTableName, collapse = " "),
#'     "
#'       ) f;
#'   ")
#'   
#'   DatabaseConnector::renderTranslateExecuteSql(
#'     connection = connection,
#'     sql = sqlCohortCreation,
#'     tempEmulationSchema = tempEmulationSchema
#'   )
#'   
#'   DatabaseConnector::renderTranslateExecuteSql(
#'     connection = connection,
#'     sql = paste0(
#'       "
#'         DROP TABLE IF EXISTS #first_exposure;
#'         DROP TABLE IF EXISTS #new_user;
#'         DROP TABLE IF EXISTS #new_user_flwd;
#'         DROP TABLE IF EXISTS #first_exposure_flwd;",
#'       paste0(
#'         " DROP TABLE IF EXISTS ",
#'         persistenceCohortDefinitionset$cohortTableName,
#'         collapse = "; "
#'       )
#'     ),
#'     tempEmulationSchema = tempEmulationSchema
#'   )
#'   
#'   sqlDrugExposureDaySupplyDistribution <- "
#'       with drug_exposures as
#'       (
#'         SELECT DISTINCT person_id,
#'                         drug_concept_id,
#'                         drug_exposure_start_date,
#'                         max(days_supply)
#'         FROM #drug_exposure de
#'         GROUP BY person_id,
#'                 drug_concept_id,
#'                 drug_exposure_start_date
#'       ),
#'       drug_sequence as
#'       (
#'         SELECT drug_concept_id,
#'                 ROW_NUMBER() OVER(PARTITION BY drug_concept_id,
#'                                   ORDER BY drug_exposure_start_date) dispensation_sequence
#'         FROM drug_exposures
#'         GROUP BY drug_concept_id
#'       )
#'       SELECT dispensation_sequence,
#'             drug_concept_id
#'       FROM drug_sequence
#'       GROUP BY dispensation_sequence,
#'           drug_concept_id;
#'   "
#'   
#' }
