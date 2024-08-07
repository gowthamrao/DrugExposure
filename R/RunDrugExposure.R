#' Run Drug Exposure Analysis
#'
#' This function takes as a input a Circe compatible concept set expression (as r list object that
#' can be converted to json), a denominator cohort or a set of rules to create the denominator
#' cohort, and checks for occurrence of drug exposure events in the drug_exposure table of
#' the CDM for the conceptId in the given concept expression in the period and for the subjects in
#' the denominator cohort. It then computes a series of drug utilization
#' metrics (adherence, persistence, utilization, patterns)
#' and reports returns a list of objects that maybe utilized in a drug exposure report.
#'
#' @template ConnectionDetails
#' @template Connection
#' @template ConceptSetExpression
#' @template QuerySource
#' @template CdmDatabaseSchema
#' @template VocabularyDatabaseSchema
#' @template DenominatorCohortDatabaseSchema
#' @template DenominatorCohortTable
#' @template DenominatorCohortId
#' @template TempEmulationSchema
#' @template GapDays
#' @template MaxFollowUpDays
#' @param forceMinimumDaysSupply (Default 1, i.e. not used). Acceptable values are NULL, or any integer value to represent days.
#'
#' @export
runDrugExposure <- function(connectionDetails = NULL,
                            connection = NULL,
                            conceptSetExpression,
                            querySource = TRUE,
                            cdmDatabaseSchema,
                            denominatorCohortDatabaseSchema,
                            denominatorCohortTable,
                            denominatorCohortId,
                            forceMinimumDaysSupply = 1,
                            maxFollowUpDays = 365,
                            vocabularyDatabaseSchema = cdmDatabaseSchema,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                            gapDays = c(0)) {
  print(
    " to do maxFollowUpDays. denominator cohort should be truncated to maxFollowUpDays. report both denominator cohort and the denominator cohort modified by days."
  )
  
  denominatorCohortDatabaseSchemaCohortTable <-
    if (is.null(denominatorCohortDatabaseSchema)) {
      denominatorCohortTable
    } else {
      paste0(denominatorCohortDatabaseSchema,
             ".",
             denominatorCohortTable)
    }
  
  checkmate::assertIntegerish(
    x = gapDays,
    lower = 0,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE
  )
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  writeLines("Running SQL...")
  createCodeSetTableFromConceptSetExpression(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    conceptSetTable = "#concept_sets"
  )
  
  getDrugExposureInDenominatorCohort(
    connection = connection,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    conceptSetTable = "#concept_sets",
    denominatorCohortTable = denominatorCohortDatabaseSchemaCohortTable,
    denominatorCohortId = denominatorCohortId,
    drugExposureOutputTable = "#drug_exposure",
    forceMinimumDaysSupply = forceMinimumDaysSupply,
    querySource = querySource
  )
  
  output <- c()
  
  output$resolvedConcepts <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT DISTINCT concept_id FROM #concept_sets;",
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema
    ) |>
    dplyr::tibble()
  
  ## cohortDefinitionSet----
  output$cohortDefinitionSet <-
    getNumeratorCohorts(
      connection = connection,
      cdmDatabaseSchema = cdmDatabaseSchema,
      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
      numeratorCohortTableBaseName = "#numerator",
      drugExposureTable = "#drug_exposure",
      gapDays = gapDays,
      baseCohortDefinitionId = 100
    ) |>
    dplyr::tibble() |>
    dplyr::mutate(cohortName = paste0("Numerator - ", .data$cohortName))
  
  writeLines("Downloading....")
  output$person <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "
            SELECT person_id,
                    gender_concept_id,
                    race_concept_id,
                    ethnicity_concept_id,
                    year_of_birth
            FROM @cdm_database_schema.person p
            INNER JOIN (
                        SELECT DISTINCT subject_id
                        FROM @denominator_cohort_table
                        WHERE cohort_definition_id = @denominator_cohort_id
                      ) d
            ON p.person_id = d.subject_id;",
    cdm_database_schema = cdmDatabaseSchema,
    denominator_cohort_id = denominatorCohortId,
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    denominator_cohort_table = denominatorCohortDatabaseSchemaCohortTable
  ) |>
    dplyr::tibble()
  
  ## code sets ----
  output$concept <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT c.*
            FROM (
                    SELECT DISTINCT concept_id
                    FROM
                    (
                      SELECT concept_id
                      FROM #concept_sets
                      UNION ALL
                      SELECT drug_concept_id
                      FROM #drug_exposure
                      UNION ALL
                      SELECT drug_source_concept_id
                      FROM #drug_exposure
                      UNION ALL
                      SELECT DISTINCT gender_concept_id
                      FROM @cdm_database_schema.person
                      UNION ALL
                      SELECT DISTINCT race_concept_id
                      FROM @cdm_database_schema.person
                      UNION ALL
                      SELECT DISTINCT ethnicity_concept_id
                      FROM @cdm_database_schema.person
                    ) combined_concepts
                  ) co
            INNER JOIN @cdm_database_schema.concept c
            ON co.concept_id = c.concept_id;",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema
  ) |>
    dplyr::tibble()
  
  
  ##persons in the observation period on cohort_start_date----
  ### by days----
  output$personsObservedDays <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = " SELECT  c.cohort_start_date,
                    COUNT(DISTINCT o.person_id) AS num_people
              FROM @denominator_cohort_table c
              JOIN @cdm_database_schema.observation_period o
              ON c.subject_id = o.person_id
              WHERE o.observation_period_start_date <= c.cohort_start_date AND
                    o.observation_period_end_date >= c.cohort_start_date
                    AND c.cohort_definition_id = @denominator_cohort_id 
              GROUP BY c.cohort_start_date;
    ",
      denominator_cohort_table = denominatorCohortDatabaseSchemaCohortTable,
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      cdm_database_schema = cdmDatabaseSchema,
      denominator_cohort_id = denominatorCohortId
    ) |>
    dplyr::tibble()
  
  output$personsObservedDaysSts <-
    processTimeSeries(
      df = output$personsObservedDays,
      dateField = "cohortStartDate",
      weight = "numPeople",
      timeRepresentations = "Day"
    )
  
  ### by month----
  output$personsObservedMonth <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = " SELECT  DATEFROMPARTS(YEAR(c.cohort_start_date), MONTH(c.cohort_start_date), 1) cohort_start_date,
                    COUNT(DISTINCT o.person_id) AS num_people
            FROM @denominator_cohort_table c
            JOIN @cdm_database_schema.observation_period o
            ON c.subject_id = o.person_id
            WHERE o.observation_period_start_date <= c.cohort_start_date AND
                  o.observation_period_end_date >= c.cohort_start_date AND
                  c.cohort_definition_id = @denominator_cohort_id
            GROUP BY DATEFROMPARTS(YEAR(c.cohort_start_date), MONTH(c.cohort_start_date), 1);
    ",
      denominator_cohort_table = denominatorCohortDatabaseSchemaCohortTable,
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      cdm_database_schema = cdmDatabaseSchema,
      denominator_cohort_id = denominatorCohortId
    ) |>
    dplyr::tibble()
  
  output$personsObservedMonthSts <-
    processTimeSeries(
      df = output$personsObservedMonth,
      dateField = "cohortStartDate",
      weight = "numPeople",
      timeRepresentations = "Month"
    )
  
  ### by quarter----
  output$personsObservedQuarter <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = " SELECT  DATEFROMPARTS(YEAR(c.cohort_start_date), 1 + 3 * ((MONTH(c.cohort_start_date) - 1) / 3), 1) cohort_start_date,
                    COUNT(DISTINCT o.person_id) AS num_people
            FROM @denominator_cohort_table c
            JOIN @cdm_database_schema.observation_period o
            ON c.subject_id = o.person_id
            WHERE o.observation_period_start_date <= c.cohort_start_date AND
                  o.observation_period_end_date >= c.cohort_start_date AND
                  c.cohort_definition_id = @denominator_cohort_id
            GROUP BY DATEFROMPARTS(YEAR(c.cohort_start_date), 1 + 3 * ((MONTH(c.cohort_start_date) - 1) / 3), 1);
    ",
      denominator_cohort_table = denominatorCohortDatabaseSchemaCohortTable,
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      cdm_database_schema = cdmDatabaseSchema,
      denominator_cohort_id = denominatorCohortId
    ) |>
    dplyr::tibble()
  
  output$personsObservedQuarterSts <-
    processTimeSeries(
      df = output$personsObservedQuarter,
      dateField = "cohortStartDate",
      weight = "numPeople",
      timeRepresentations = "Quarter"
    )
  
  ### by year----
  output$personsObservedYear <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = " SELECT  DATEFROMPARTS(YEAR(c.cohort_start_date), 1 + 3 * ((MONTH(c.cohort_start_date) - 1) / 3), 1) cohort_start_date,
                    COUNT(DISTINCT o.person_id) AS num_people
            FROM @denominator_cohort_table c
            JOIN @cdm_database_schema.observation_period o
            ON c.subject_id = o.person_id
            WHERE o.observation_period_start_date <= c.cohort_start_date AND
                  o.observation_period_end_date >= c.cohort_start_date AND
                  c.cohort_definition_id = @denominator_cohort_id
            GROUP BY DATEFROMPARTS(YEAR(c.cohort_start_date), 1 + 3 * ((MONTH(c.cohort_start_date) - 1) / 3), 1);
    ",
      denominator_cohort_table = denominatorCohortDatabaseSchemaCohortTable,
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      cdm_database_schema = cdmDatabaseSchema,
      denominator_cohort_id = denominatorCohortId
    ) |>
    dplyr::tibble()
  
  output$personsObservedYearSts <-
    processTimeSeries(
      df = output$personsObservedYear,
      dateField = "cohortStartDate",
      weight = "numPeople",
      timeRepresentations = "Year"
    )
  
  ## denominator----
  output$denominator <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM @cohort_table
            WHERE cohort_definition_id = @denominator_cohort_id;",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    cohort_table = denominatorCohortDatabaseSchemaCohortTable,
    denominator_cohort_id = denominatorCohortId
  ) |>
    dplyr::tibble()
  
  output$denominatorSts <-
    processTimeSeries(df = output$denominator, dateField = "cohortStartDate")
  
  ## get drug exposure full -----
  output$drugExposure <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM #drug_exposure;",
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  ) |>
    dplyr::tibble()
  
  ## get drug exposure summary ----
  sqlDrugExposureDays <- "
                      select person_id,
                          drug_exposure_start_date,
                        	SUM(CASE WHEN DATEADD(day, 
                        	        DAYS_SUPPLY, 
                        	        DRUG_EXPOSURE_START_DATE) > cohort_end_date THEN
                        	    DATEDIFF(day, DRUG_EXPOSURE_START_DATE, DRUG_EXPOSURE_END_DATE) + 1 ELSE days_supply END
                        	    ) days_supply
                      from @denominator_cohort_table c
                      inner join #drug_exposure de
                      on c.subject_id = de.person_id
                      	and c.cohort_start_date <= drug_exposure_start_date
                      	and c.cohort_end_Date >= drug_exposure_start_date
                      WHERE c.cohort_definition_id = @denominator_cohort_id
                        AND drug_exposure_start_date >= cohort_start_date
                        AND drug_exposure_end_date <= cohort_end_date
                      GROUP BY person_id, drug_exposure_start_date
                      ORDER BY person_id, drug_exposure_start_date;
                      "
  output$drugExposureDays <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sqlDrugExposureDays,
      denominator_cohort_table = denominatorCohortDatabaseSchemaCohortTable,
      denominator_cohort_id = denominatorCohortId,
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema
    )
  
  ## drug exposure cohort 1----
  drugExposureCohort1 <- output$drugExposure |>
    dplyr::mutate(cohortDefinitionId = 1) |> 
    dplyr::rename(subjectId = .data$personId,
                  cohortStartDate = .data$drugExposureStartDate,
                  cohortEndDate = .data$drugExposureEndDate) |>
    dplyr::select(
      .data$cohortDefinitionId,
      .data$subjectId,
      .data$cohortStartDate,
      .data$cohortEndDate
    ) |>
    dplyr::arrange(
      .data$cohortDefinitionId,
      .data$subjectId,
      .data$cohortStartDate,
      .data$cohortEndDate
    )
  
  ## drug exposure cohort 2----
  drugExposureCohort2 <- output$drugExposureDays |> 
    dplyr::mutate(cohortDefinitionId = 2,
                  drugExposureEndDate = .data$drugExposureStartDate + .data$daysSupply) |> 
    dplyr::rename(subjectId = .data$personId,
                  cohortStartDate = .data$drugExposureStartDate,
                  cohortEndDate = .data$drugExposureEndDate) |>
    dplyr::select(
      .data$cohortDefinitionId,
      .data$subjectId,
      .data$cohortStartDate,
      .data$cohortEndDate
    ) |>
    dplyr::arrange(
      .data$cohortDefinitionId,
      .data$subjectId,
      .data$cohortStartDate,
      .data$cohortEndDate
    )
  
  drugExposureCohort <- dplyr::bind_rows(drugExposureCohort1,
                                         drugExposureCohort2)
  
  ## get numerator ----
  numeratorCohorts <- c()
  for (i in (1:nrow(output$cohortDefinitionSet))) {
    x1 <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM ",
        output$cohortDefinitionSet[i,]$cohortTableName,
        ";"
      ),
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema
    ) |> dplyr::tibble()
    
    x2 <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT 1000 + cohort_definition_id AS cohort_definition_id,
                subject_id,
                min(cohort_start_date) cohort_start_date,
                min(cohort_end_date) cohort_end_date
          FROM ",
        output$cohortDefinitionSet[i,]$cohortTableName,
        " GROUP BY cohort_definition_id, subject_id;"
      ),
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema
    ) |> dplyr::tibble()
    
    numeratorCohorts[[i]] <- dplyr::bind_rows(x1, x2)
  }
  
  output$cohortDefinitionSet <- dplyr::bind_rows(
    output$cohortDefinitionSet,
    output$cohortDefinitionSet |>
      dplyr::mutate(
        cohortId = (1000 + .data$cohortId),
        cohortName = paste0(.data$cohortName,
                            " earliest event")
      ),
    dplyr::tibble(
      cohortId = denominatorCohortId,
      cohortName = 'Denominator',
      persistenceDay = 0,
      cohortTableName = if (!is.null(denominatorCohortDatabaseSchema)) {
        paste0(denominatorCohortDatabaseSchema,
               ".",
               denominatorCohortTable)
      } else {
        denominatorCohortTable
      }
    ),
    dplyr::tibble(
      cohortId = 1,
      cohortName = 'DrugExposure',
      persistenceDay = 0,
      cohortTableName = "cdm.drug_exposure"
    ),
    dplyr::tibble(
      cohortId = 2,
      cohortName = 'DrugExposure with right censor (max days)',
      persistenceDay = 0,
      cohortTableName = "cdm.drug_exposure"
    )
  )
  
  output$numeratorCohorts <- dplyr::bind_rows(numeratorCohorts)
  
  output$cohorts <- dplyr::bind_rows(output$numeratorCohorts,
                                     output$denominator,
                                     drugExposureCohort)
  
  ## cohort days ----
  output$cohortDays <- output$cohorts |>
    dplyr::group_by(.data$cohortDefinitionId) |>
    dplyr::summarize(
      persons = n_distinct(.data$subjectId),
      events = n(),
      days = as.double(sum(.data$cohortEndDate - .data$cohortStartDate + 1))
    ) |>
    dplyr::rename(cohortId = .data$cohortDefinitionId) |>
    dplyr::inner_join(
      output$cohortDefinitionSet |>
        dplyr::select(.data$cohortId,
                      .data$cohortName),
      by = "cohortId"
    )
  
  
  ## drug persistence proportion----
  # Create a sequence of months
  daysAsmonth <-
    seq(from = 30,
        by = 30,
        length.out = ceiling((maxFollowUpDays - 30) / 30) + 1)
  proportionDays <-
    c(maxFollowUpDays * (seq(0, 100, by = 5) / 100)) |> floor() |> unique()
  daysAsWeek <-
    seq(from = 7,
        by = 7,
        length.out = ceiling((maxFollowUpDays - 7) / 7) + 1)
  proportionDays <-
    c(maxFollowUpDays * (seq(0, 100, by = 5) / 100)) |> floor() |> unique()
  thresholdDays <-
    c(daysAsmonth, proportionDays, daysAsWeek, 100, 200, 300, 400, 500) |> unique() |> sort()
  
  # Cartesian product of distinct cohort_definition_id and months
  combis <- output$cohortDefinitionSet |>
    dplyr::select(.data$cohortId) |>
    dplyr::rename(cohortDefinitionId = .data$cohortId) |>
    dplyr::distinct() |>
    tidyr::expand(.data$cohortDefinitionId, thresholdDays)

  # Summing days and calculating floor of months
  output$drugPersistenceProportion <-
    output$cohorts |>
    dplyr::mutate(days = as.integer(.data$cohortEndDate - .data$cohortStartDate + 1)) |>
    dplyr::group_by(.data$cohortDefinitionId, .data$subjectId) |>
    dplyr::summarise(sumDays = sum(.data$days),
                     .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::mutate(sumDays = dplyr::if_else(condition = .data$sumDays > maxFollowUpDays, 
                                           true = maxFollowUpDays, 
                                           false = .data$sumDays)) |>
    dplyr::inner_join(combis,
                      by = "cohortDefinitionId", relationship = "many-to-many") |>
    dplyr::filter(thresholdDays <= .data$sumDays) |>
    dplyr::group_by(.data$cohortDefinitionId, .data$thresholdDays) |>
    dplyr::summarise(
      personWithPersistentExposure = dplyr::n_distinct(.data$subjectId),
      .groups = "drop"
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      output$cohorts |>
        dplyr::group_by(.data$cohortDefinitionId) |>
        dplyr::summarise(totalPersons = dplyr::n_distinct(.data$subjectId)),
      by = "cohortDefinitionId"
    ) |>
    dplyr::mutate(persistenceProportion = .data$personWithPersistentExposure / .data$totalPersons) |>
    dplyr::select(
      .data$cohortDefinitionId,
      .data$thresholdDays,
      .data$personWithPersistentExposure,
      .data$persistenceProportion
    ) |>
    dplyr::rename(cohortId = .data$cohortDefinitionId) |>
    dplyr::inner_join(output$cohortDefinitionSet |>
                        dplyr::select(.data$cohortId,
                                      .data$cohortName),
                      by = "cohortId") |>
    dplyr::mutate(cohortNameCohortId = gsub(
      pattern = "-",
      replacement = "\n",
      x = .data$cohortName
    )) |>
    dplyr::relocate(.data$cohortId,
                    .data$cohortName) |>
    dplyr::arrange(.data$cohortId,
                   .data$cohortName)
  
  output$drugPersistenceProportionGraph <-
    ggplot2::ggplot(
      data = output$drugPersistenceProportion,
      ggplot2::aes(x = thresholdDays,
                   y = .data$persistenceProportion)
    ) +
    ggplot2::geom_line() +  # Use geom_line to connect points
    ggplot2::facet_wrap(~ cohortNameCohortId, scales = "free_y") +
    ggplot2::theme_minimal() +  # Optional: a minimal theme
    ggplot2::labs(title = "Persistence Proportion by Threshold Days",
                  x = "Threshold Days",
                  y = "Persistence Proportion")
  
  drugAdherenceDays <- output$cohorts |>
    dplyr::group_by(.data$cohortDefinitionId,
                    .data$subjectId) |>
    dplyr::summarise(days = sum(.data$cohortEndDate - .data$cohortStartDate + 1) |> as.double(), .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::select(.data$cohortDefinitionId,
                  .data$days) |>
    dplyr::inner_join(
      output$cohortDefinitionSet |>
        dplyr::select(.data$cohortId,
                      .data$cohortName) |>
        dplyr::rename(cohortDefinitionId = .data$cohortId),
      by = "cohortDefinitionId"
    )
  
  output$drugAdherence <- drugAdherenceDays |>
    calculateSummaryStatistics(value = "days", group = "cohortDefinitionId") |> 
    dplyr::rename(cohortDefinitionId = .data$group)
  
  output$drugAdherenceRightCensored <- drugAdherenceDays |>
    dplyr::mutate(days = dplyr::if_else(
      condition = .data$days > maxFollowUpDays,
      true = maxFollowUpDays,
      false = .data$days
    )) |>
    calculateSummaryStatistics(value = "days", 
                               group = "cohortDefinitionId") |> 
    dplyr::rename(cohortDefinitionId = .data$group)
  
  output$drugAherencePlot <-
    createViolinPlot(data = drugAdherenceDays,
                     xName = "cohortName",
                     yName = "days")
  
  output$drugAherencePlotRightCensored <-
    createViolinPlot(
      data = drugAdherenceDays |>
        dplyr::mutate(
          days = dplyr::if_else(
            condition = .data$days > maxFollowUpDays,
            true = maxFollowUpDays,
            false = .data$days
          )
        ),
      xName = "cohortName",
      yName = "days"
    )
  
  writeLines("Done....")
  return(output)
}
