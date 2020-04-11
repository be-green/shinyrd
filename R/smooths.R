# Methods for Kernel Regression & Local Linear Regression

#' Estimate bandwidth for regression discontinuity
#' @param rv running variable
#' @param cutoff cutoff(s) for discontinuity
#' @param y outcome variable
#' @details
#' For the final bandwidth for the regression discontinuity
#' design, you should include a y variable to choose
#' Imben's criteria (see [bandwidth_imbens]). If you are
#' estimating a bandwidth to be used in a sorting test,
#' to make sure people aren't sorting on the running variable,
#' you omit y entirely.
#' @export
guess_bandwidth <- function(rv, cutoff, y = NULL, method) {
  do.call(paste0("bandwidth_", method),
          args = list(rv = rv,
                      cutoff = cutoff,
                      y = y))
}

#' Choose bandwidth for density test based on McCrary's Criterion
#' @import magrittr
#' @param rv running variable
#' @param cutoff cutoff(s) for discontinuity
#' @param method method for selecting bin width for first-step histogram smoother
#' @param ... ignored for now
#' @references McCrary, Justin. "Manipulation of the running variable in the
#' regression discontinuity design: A density test."
#' Journal of econometrics 142.2 (2008): 698-714.
#' @details The procedure as detailed in McCrary (2008) recommends
#' using an automated process as a starting point and subsequently
#' implementing a subjective rule, while reporting the difference
#' from the automated process. For details, please see the paper.
#'
#' For details on the first-step histogram smoother, see [make_grid]
#' or [bin].
#' @export
bandwidth_mccrary <- function(rv, cutoff, method = "mccrary", ...) {
  grid <- make_grid(rv, cutoff, method = method)

  split(grid, by = "group") %>%
    sapply(function(x) calc_mccrary_rule(x[["bin_median"]],
                                         x[["freq"]],
                                         cutoff = cutoff)) %>%
    mean

}

#' Calculate mccrary density rule
#' @importFrom stats lm
#' @importFrom stats poly
#' @param x x variable, bin midpoints from [make_grid]
#' @param y y variable, frequency of observations in those groups
#' @param cutoff cutoff to be used for the rdd design
calc_mccrary_rule <- function(x, y, cutoff) {

  fit <- stats::lm(y ~ stats::poly(x, 4, raw = T))

  second_deriv <- function(x, fit) {
    2 * coef(fit)[3] +
      6 * coef(fit)[4] * x +
      12 * coef(fit)[5] * x^2
  }

  if(max(x) < cutoff) {
    b <- min(x)
  } else {
    b <- max(x)
  }

  mse <- mean((fit$residuals)^2)

  3.348 * (
    (mse * abs(b - cutoff)) /
     sum(
       sapply(x,
              second_deriv,
              fit = fit)^2)
    )^(1/5)
}

#' Choose bandwidth for regression discontintuity
#' based on Imben's Criterion
#' @param rv running variable
#' @param cutoff cutoff(s) for discontinuity
#' @param y outcome variable
bandwidth_imbens <- function(rv, y) {

}
