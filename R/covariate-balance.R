
#' Visualize covariate balance at the discontinuity
#' @import magrittr
#' @import ggplot2
#' @import data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table `:=`
#' @importFrom ggplot2 ggplot
#' @param data Data to use for covariate balance checks
#' @param running_variable running variable to use for checks
#' @param cov Vector of covariate names to plot
#' @param method Method to use for balance checks
#' @param ... other parameters to pass to function
#' @details The method argument defaults to a linear regression
#' using all the data. Other acceptable arguments are "loess"
#' for local linear regression, "gam" for generalized additive
#' models, "quadratic" for a quadratic fit using the whole
#' dataset.
#' @export

plot_covbal <- function(data,
                        method = c("lm", "loess", "gam", "quad"),
                        running_variable,
                        cov,
                        binwidth = NULL,
                        cutoff,
                        ...) {

  if(is.null(binwidth)) {
    binwidth <- guess_binwidth(data[[running_variable]])
  }

  data <- as.data.table(data)
  data[,Group := findInterval(get(running_variable), cutoff)]

  plot_data <- data[,.SD, .SDcols = c(
    "Group",
    running_variable,
    cov
  )]

  plot_data[,Interval := findInterval(
    get(running_variable),
    seq(0, max(Group + 1), by = binwidth)
  )]

  bin_lvls <- seq(min(data[[running_variable]]),
                  max(data[[running_variable]]),
                  by = binwidth)

  plot_data <- merge(plot_data,
                     data.table(Interval = 1:length(bin_lvls),
                                Level = bin_lvls),
                           by = "Interval")

  melt(plot_data, id.vars = c("Group", "Interval", running_variable, "Level"),
                    variable.name = "Variable",
                    value.name = "Value",
                    ) %>%
    ggplot2::ggplot(data = .,
                    ggplot2::aes_string(x = "Level",
                                        y = "Value",
                                        group = "Group")) +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::stat_summary(fun.y = mean, geom = "point") +
    facet_wrap(~Variable, scales = 'free_y') +
    xlab("Level of Running Variable") +
    ylab("Value of Covariate") +
    geom_vline(xintercept = cutoff)

}

