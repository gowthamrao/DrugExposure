#' Calculate summary statistics
#'
#' This function, given a data frame (df) with a continuous variable and a group,
#' computes distribution statistics.
#'
#' @param df Data frame to perform calculations on
#' @param value Field in the data frame that has the data to calculate.
#' @param group Any optional grouping variable
calculateSummaryStatistics <-
  function(df, value = "value", group = NULL) {
    if (!is.null(group)) {
      # Ensure correct package references
      dataFrame <- df |>
        dplyr::select(all_of(c(group, value))) |>
        dplyr::group_by(all_of(group))
    } else {
      # Ensure correct package references
      dataFrame <- df |>
        dplyr::select(all_of(value))
    }
    # Helper function to calculate mode
    calculateMode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    output <- dataFrame |>
      dplyr::summarize(
        mean = mean(all_of(value), na.rm = TRUE),
        sd = sd(all_of(value), na.rm = TRUE),
        median = median(all_of(value), na.rm = TRUE),
        p01 = quantile(all_of(value), 0.01, na.rm = TRUE),
        p05 = quantile(all_of(value), 0.05, na.rm = TRUE),
        p25 = quantile(all_of(value), 0.25, na.rm = TRUE),
        p75 = quantile(all_of(value), 0.75, na.rm = TRUE),
        p95 = quantile(all_of(value), 0.95, na.rm = TRUE),
        p99 = quantile(all_of(value), 0.99, na.rm = TRUE),
        mode = calculateMode(all_of(value)),
        count = dplyr::n(),
        count_distinct = dplyr::n_distinct(all_of(value))
      )
    
    if (!is.null(group)) {
      output <- output |>
        tidyr::pivot_longer(
          cols = -all_of(group),
          names_to = "statistic",
          values_to = "value"
        )
    }
    
    return(output)
  }
