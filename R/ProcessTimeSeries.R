#' Process Dates for STL Time Series Analysis
#'
#' This function performs STL decomposition on a data frame for specified time representations.
#' It requires a date field and can optionally use a weight field to aggregate data. The function
#' handles different time aggregations (e.g., Day, Month, Quarter, Year) and returns a list of
#' models, components, and plots for each time representation.
#'
#' @param df A data frame containing the time series data.
#' @param timeRepresentations A character vector specifying the time units to aggregate by.
#'                            Accepted values are 'Day', 'Month', 'Quarter', 'Year'.
#'                            Default is c('Day', 'Month', 'Quarter', 'Year').
#' @param dateField The name of the column in `df` that contains the date information.
#'                  Defaults to 'cohortStartDate'.
#' @param weight An optional name of the column that contains weights for aggregation.
#'               If NULL, each record is assumed to have an equal weight of 1.
#'
#' @return A list where each element corresponds to a specified time representation and
#'         contains the processed data, the fitted model, its components, and a plot of the
#'         decomposition.
#'
#' @export
processTimeSeries <-
  function(df,
           timeRepresentations = c('Day', 'Month', 'Quarter', 'Year'),
           dateField = 'cohortStartDate',
           weight = NULL) {
    # Check input validity
    checkmate::assertDataFrame(df)
    checkmate::assertCharacter(dateField, len = 1, min.chars = 1)
    checkmate::assertCharacter(
      timeRepresentations,
      min.len = 1,
      any.missing = FALSE,
      min.chars = 1
    )
    checkmate::assertChoice(timeRepresentations,
                            choices = c('Day', 'Month', 'Quarter', 'Year'))
    if (!is.null(weight)) {
      checkmate::assertCharacter(weight, len = 1, min.chars = 1)
      checkmate::assertColumnExists(df, weight)
    }
    checkmate::assertColumnExists(df, dateField)
    
    if (!is.null(weight)) {
      df <- df |>
        dplyr::select(.data[[dateField]],
                      .data[[weight]]) |>
        dplyr::mutate(dateField = .data[[dateField]],
                      weight = .data[[weight]]) |>
        dplyr::group_by(dateField) |>
        dplyr::summarise(weight = sum(weight))
    } else {
      df <- df |>
        dplyr::select(.data[[dateField]]) |>
        dplyr::mutate(dateField = .data[[dateField]],
                      weight = 1) |>
        dplyr::group_by(dateField) |>
        dplyr::summarise(weight = sum(weight))
    }
    
    result <- list()
    
    for (timeRep in timeRepresentations) {
      # Data handling and conversion to regular tsibble
      if (timeRep == 'Day') {
        processedData <- data |>
          dplyr::select(dateField, weight) |>
          dplyr::rename(timeGroup = dateField)
      } else if (timeRep == 'Month') {
        processedData <- data |>
          dplyr::select(dateField) |>
          dplyr::mutate(timeGroup = tsibble::yearmonth(dateField)) |>
          dplyr::select(dateField, weight)
      } else if (timeRep == 'Quarter') {
        processedData <- data |>
          dplyr::select(dateField) |>
          dplyr::mutate(timeGroup = tsibble::yearquarter(dateField)) |>
          dplyr::select(dateField, weight)
      } else if (timeRep == 'Week') {
        processedData <- data |>
          dplyr::select(dateField) |>
          dplyr::mutate(timeGroup = tsibble::yearweek(dateField)) |>
          dplyr::select(timeGroup)
      } else if (timeRep == 'Year') {
        processedData <- data |>
          dplyr::select(dateField) |>
          dplyr::mutate(timeGroup = format(dateField, "%Y") |> as.integer()) |>
          dplyr::select(dateField, weight)
      }
      
      processedData <- processedData |>
        dplyr::group_by(timeGroup) |>
        dplyr::summarise(weight = dplyr::sum(weight)) |>
        tsibble::as_tsibble(index = timeGroup) |>
        tsibble::fill_gaps(weight = 0)
      
      # Calculate model
      model <- processedData |>
        fabletools::model(feasts::STL(count ~ season(window = 'periodic')))
      
      # Extract components and plot
      components <- model |> fabletools::components()
      plot <- components |> feasts::autoplot()
      
      # Store results in the list
      result[[timeRep]] <-
        list(
          processedData = processedData,
          model = model,
          components = components,
          plot = plot
        )
    }
    
    return(result)
  }
