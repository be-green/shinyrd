
#' Guess binwidth for histogram
#' @importFrom stats IQR
#' @param x vector to use in histogram
#' @details This uses the Freedman–Diaconis rule
#' to determine width of bins. To be used as a default
#' in case the user doesn't specify their own parameter
#' @export
guess_binwidth <- function(x) {
  2 * stats::IQR(x) / length(x)^(1/3)
}


#' Plot McCrary Density Test
#' @param data data to be used by plot
#' @param running_variable Running variable for regression
#' discontinuity design
#' @param cutoff Cutoff to be used as the discontinuity
#' @param binwidth Bin width to be used in histogram,
#' will guess via [guess_binwidth] if none is specified
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_vline
#' @details Plots a histogram with a vertical line at the
#' discontinuity. Useful to check for manipulation.
#' @examples
#' # produces a density plot
#' density_plot(hansen_dwi, "bac1", cutoff = c(0.08))
#' @export
density_plot <- function(data,
                         running_variable,
                         cutoff,
                         binwidth = NULL,
                         title = NULL,
                         subtitle = NULL,
                         theme = NULL,
                         ...) {

  if(is.null(binwidth)) {
    binwidth <- guess_binwidth(data[[running_variable]])
    message("No bin width specified, using ",
            signif(binwidth, 2),
            " by default.")
  }

  if(is.null(title)) {
    title <- paste("Density Plot of", running_variable)
  }

  if(is.null(subtitle)) {
    subtitle <- paste("Discontinuity Cutoff(s):",
                      paste0(cutoff, collapse = ", "))
  }


  ggplot2::ggplot(data,
                  ggplot2::aes_string(x = running_variable)) +
    ggplot2::geom_histogram(binwidth = binwidth) +
    ggplot2::geom_vline(xintercept = cutoff) +
    ggplot2::ggtitle(label = title,
                     subtitle = subtitle)
}

