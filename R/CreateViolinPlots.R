#' Create an Enhanced Violin Plot
#'
#' This function generates a violin plot enhanced with overlaid boxplots and
#' symbols for mean and median values. It uses ggplot2 for plotting, viridis for
#' color palettes, and hrbrthemes for themes.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param xName The name of the categorical variable (as a string) to plot on the x-axis.
#' @param yName The name of the numeric variable (as a string) to plot on the y-axis.
#' @param fillName (Optional) The name of the variable (as a string) to use for fill colors. Default is NULL.
#' @param title The title of the plot. Default is "Distribution Plot".
#' @param colorMean The color to use for the mean symbol. Default is "blue".
#' @param colorMedian The color to use for the median symbol. Default is "green".
#' @param sizeMean The size of the mean symbol. Default is 3.
#' @param sizeMedian The size of the median symbol. Default is 3.
#' @param shapeMean The shape of the mean symbol. Default is 18.
#' @param shapeMedian The shape of the median symbol. Default is 16.
#' @param angleXLabels What is angle for the x tick labels.
#'
#' @return A ggplot object representing the violin plot.
#'
createViolinPlot <-
  function(data,
           xName = "groupLabel",
           yName = "value",
           fillName = NULL,
           title = "Distribution Plot",
           colorMean = "black",
           colorMedian = "black",
           sizeMean = 3,
           sizeMedian = 3,
           shapeMean = 18, # square
           shapeMedian = 16, # solid circle
           angleXLabels = 45 # Angle for x-axis labels, default is 45 degrees
  ) {
    # Define aesthetics dynamically
    aesString <- if (!is.null(fillName)) {
      ggplot2::aes_string(x = xName, y = yName, fill = fillName)
    } else {
      ggplot2::aes_string(x = xName, y = yName, fill = xName)
    }
    
    plot <- data |>
      ggplot2::ggplot(aesString) +
      ggplot2::geom_violin(width = 1.4,
                           adjust = 1,
                           alpha = 0.7) +
      ggplot2::geom_boxplot(
        width = 0.1,
        color = "grey",
        alpha = 0.2,
        outlier.shape = NA
      ) +
      viridis::scale_fill_viridis(discrete = TRUE, option = "D") +
      hrbrthemes::theme_ipsum(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        plot.background = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = angleXLabels, hjust = 1)
      ) +
      ggplot2::ggtitle(title) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::stat_summary(
        fun = mean,
        geom = "point",
        color = colorMean,
        shape = shapeMean,
        size = sizeMean,
        show.legend = TRUE
      ) +
      ggplot2::stat_summary(
        fun = stats::median,
        geom = "point",
        color = colorMedian,
        shape = shapeMedian,
        size = sizeMedian,
        show.legend = TRUE
      )
    
    return(plot)
  }
