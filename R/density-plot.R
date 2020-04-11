#' Plot Density Histogram
#' @param data data to be used by plot
#' @param rv Running variable for regression
#' discontinuity design
#' @param cutoff Cutoff to be used as the discontinuity
#' @param binwidth Bin width to be used in histogram,
#' will guess via [guess_binwidth] if none is specified
#' @param method method to be used for guessing binwidth
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
plot_hist <- function(data,
                      rv,
                      cutoff,
                      binwidth = NULL,
                      method = "mccrary",
                      title = NULL,
                      subtitle = NULL,
                      theme = NULL,
                      ...) {

  if(is.null(binwidth)) {
    binwidth <- guess_binwidth(data[[rv]], method = method)
    message("No bin width specified, using ",
            signif(binwidth, 2),
            " by default.")
  }

  if(is.null(title)) {
    title <- paste("Density Plot of", rv)
  }

  if(is.null(subtitle)) {
    subtitle <- paste("Discontinuity Cutoff(s):",
                      paste0(cutoff, collapse = ", "))
  }


  ggplot2::ggplot(data,
                  ggplot2::aes_string(x = rv)) +
    ggplot2::geom_histogram(binwidth = binwidth) +
    ggplot2::geom_vline(xintercept = cutoff) +
    ggplot2::ggtitle(label = title,
                     subtitle = subtitle)
}

#' Plot for McCrary sorting test
#' @param data data to be used by plot
#' @param rv name of column of running variable
#' @param cutoff cutoff to be tested for sorting
#' @param method currently ignored
#' @param binwidth binwidth to be used for first-stage
#' histogram smoother
#' @param bandwidth bandwidth to be used for second-stage
#' kernel smoother
#' @param title Title for the plot
#' @param subtitle subtitle for the plot
#' @param theme theme for the plot
#' @param ... other arguments to be passed
#' @export
plot_mccrary <- function(data,
                         rv,
                         cutoff,
                         method = NULL,
                         binwidth = NULL,
                         bandwidth = NULL,
                         bin_method = "mccrary",
                         band_method = "mccrary",
                         title = NULL,
                         subtitle = NULL,
                         theme = NULL,
                         ...) {

  if(is.null(binwidth)) {
    binwidth <- guess_binwidth(data[[rv]], method = bin_method)
    message("No bin width specified, using ",
            signif(binwidth, 2),
            " by default.")
  }

  if(is.null(bandwidth)) {
    bandwidth <- guess_bandwidth(data[[rv]], cutoff = cutoff,
                                 method = band_method)
    message("No bandwidth specified, using ",
            signif(bandwidth, 2),
            " by default.")
  }


  if(is.null(title)) {
    title <- paste("Density Plot of", rv)
  }

  if(is.null(subtitle)) {
    subtitle <- paste("Discontinuity Cutoff(s):",
                      paste0(cutoff, collapse = ", "))
  }

  data <- as.data.table(data)
  plot_data <- make_grid(data[[rv]],
                         cutoff = cutoff, binwidth = binwidth)

  ggplot2::ggplot(plot_data,
                  ggplot2::aes_string(x = "bin_median",
                                      y = "freq",
                                      group = "group")) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = cutoff) +
    ggplot2::ggtitle(label = title,
                     subtitle = subtitle) +
    ggplot2::geom_smooth(method = "loess")
}


