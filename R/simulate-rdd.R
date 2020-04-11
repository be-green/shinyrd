#' Simulate regression discontinuity datasets given fitted model
#' @param object object fit with RDRobust, rdd, rdtools, or just linear regression
#' @param ...
#' @details This function accepts a fit that estimates a regression discontinuity,
#' and then assumes that the fitted model represents the true data generating
#' process. Then it simulates several datasets given that process, returning
#' them in a data.table well suited to accompanying visual checks.
simulate_rd <- function(object, ...) {
  UseMethod("simulate_rd")
}

simulate_rd.lm <- function(object, ...) {

}

