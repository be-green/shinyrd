
#' Visualize covariate balance at the discontinuity
#' @import magrittr
#' @import ggplot2
#' @import data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table `:=`
#' @importFrom ggplot2 ggplot
#' @param data Data to use for covariate balance checks
#' @param rv The running variable to use in the discontinuity
#' @param cov Vector of covariate names to plot
#' @param geom Representation binned points, the name of any geom.
#' See [the ggplot2 website](https://ggplot2.tidyverse.org/reference/).
#' @param fit_method Method to use to fit regression line for balance checks
#' @param aggregate Whether to aggregate observations within each bin for visualization purposes
#' @param ... other parameters to be passed to geom_smooth, the call which plots the line
#' @details
#' Covariate balance tests allow us to visualize
#' whether important characteristics differ on either side
#' of the regression discontinuity boundary.
#'
#' The sum_method indicates how data is meant to be binned
#' for visualization purposes. Currently it defaults to "raw"
#' but it will also accept "mean", "median", and "pointrange",
#' which will plot a point-range for each bin on the x axis.
#'
#' The fit_method argument defaults to a linear regression
#' using all the data. Other acceptable arguments are "loess"
#' for local linear regression, "gam" for generalized additive
#' models, "quadratic" for a quadratic fit using the whole
#' dataset.
#' @export
plot_covbal <- function(data,
                        rv,
                        cov,
                        aggregate = T,
                        geom = "point",
                        fit_method = "lm",
                        binwidth = NULL,
                        cutoff,
                        ...) {

  if(is.null(binwidth)) {
    binwidth <- guess_binwidth(data[[rv]])
  }

  data <- as.data.table(data)
  data[,Group := findInterval(get(rv), cutoff)]

  plot_data <- data[,.SD, .SDcols = c(
    "Group",
    rv,
    cov
  )]

  plot_data[,Interval := findInterval(
    get(rv),
    seq(0, max(Group + 1), by = binwidth)
  )]

  bin_lvls <- seq(min(data[[rv]]),
                  max(data[[rv]]),
                  by = binwidth)

  if(fit_method == "quad") {
    smooth_method <- "lm"
  } else {
    smooth_method <- fit_method
  }

  if(fit_method == "quad") {
    smooth_form = as.formula(y ~ x + I(x^2))
  } else {
    smooth_form = as.formula(y ~ x)
  }

  plot_data <- merge(plot_data,
                     data.table(Interval = 1:length(bin_lvls),
                                Level = bin_lvls),
                           by = "Interval")

  plot <- melt(plot_data, id.vars = c("Group", "Interval", rv, "Level"),
                    variable.name = "Variable",
                    value.name = "Value",
                    ) %>%
    ggplot2::ggplot(data = .,
                    ggplot2::aes_string(x = "Level",
                                        y = "Value",
                                        group = "Group")) +
    facet_wrap(~Variable, scales = 'free_y') +
    xlab("Level of Running Variable") +
    ylab("Value of Covariate") +
    geom_vline(xintercept = cutoff)

  if(aggregate) {
    plot <- plot + ggplot2::stat_summary(fun.y = mean,
                                 fun.ymax = function(x) mean(x) + 2*sd(x)/sqrt(length(x)),
                                 fun.ymin = function(x) mean(x) - 2*sd(x)/sqrt(length(x)),
                                 geom = geom)
  } else {
    plot <- plot + getFromNamespace(paste0("geom_", geom), ns = "ggplot2")()
  }

  plot + ggplot2::geom_smooth(method = smooth_method,
                              formula = smooth_form)

}
