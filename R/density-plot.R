

#' Plot Density Histogram
#' @param data data to be used by plot
#' @param rv Running variable for regression
#' discontinuity design
#' @param cutoff Cutoff to be used as the discontinuity
#' @param bw Bin width to be used in histogram,
#' will guess via [guess_bw] if none is specified
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
                         bw = NULL,
                         title = NULL,
                         subtitle = NULL,
                         theme = NULL,
                         ...) {

  if(is.null(bw)) {
    bw <- guess_bw(data[[rv]])
    message("No bin width specified, using ",
            signif(bw, 2),
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
    ggplot2::geom_histogram(binwidth = bw) +
    ggplot2::geom_vline(xintercept = cutoff) +
    ggplot2::ggtitle(label = title,
                     subtitle = subtitle)
}


plot_mccrary <- function(data,
                         rv,
                         cutoff,
                         method = "lm",
                         bw = NULL,
                         title = NULL,
                         subtitle = NULL,
                         theme = NULL,
                         ...) {

  if(is.null(bw)) {
    bw <- guess_bw(data[[rv]])
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

  data <- as.data.table(data)
  data[,group := findInterval(get(rv), cutoff, all.inside = T)]

  ggplot2::ggplot(data,
                  ggplot2::aes_string(x = rv,
                                      group = "group")) +
    ggplot2::geom_histogram(aes(y=..density..),
                      binwidth = bw) +
    ggplot2::geom_vline(xintercept = cutoff) +
    ggplot2::ggtitle(label = title,
                     subtitle = subtitle) +
    ggplot2::geom_smooth(method = "loess", y=..density..)
}


