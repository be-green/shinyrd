#' Perform local polynomial regression
#' @param form formula for regression to use
#' @param data data to use for regression
#' @param bw bandwidth for kernel weighting
#' @param rv running variable to use for kernel weighting
#' @param kern kernel to use for weighting observations
#' @details Performs local polynomial regression using
#' weighted least squares. Kernel must be one of the supported
#' kernel weighting schemes or your own function
#' that weights given a bandwidth.
localpoly <- function(form, data, bw, rv, kernel) {
  kernel <- tolower(kernel)
  wts <- weight(x = data[[rv]],
                    bw = bw,
                    kernel = kernel)
  lm(formula = as.formula(form), data = data,
     weights = wts)
}

#' Roll a function over a data.table given a bandwidth
#' @param h bandwith to use for kernel regression
#' @param rv running variable
#' @param data data to use in regression
#' @param kernel kernel to use in regression estimation
#' @importFrom data.table as.data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table setorderv
#' @importFrom data.table between
roll_kernel <- function(h, rv, data, kernel, ...) {
  if(anyNA(data[[rv]])) {
    warning("Dropping NAs found in the running variable.")
  }

  if(!is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  data.table::setorderv(data, rv)

  min_var <- min(data[[rv]], na.rm = T)
  max_var <- max(data[[rv]], na.rm = T)
  grid <- c(seq(min_var, max_var, by = h), max_var)

  out_list <- list()
  for(i in 1:length(grid)) {
    bottom <- min_var + (i - 1) * h
    top <- bottom + h
    dat <- data[data.table::between(get(rv), bottom, top)]
    if(nrow(dat) == 0) {
      out_list[[i]] <- list(
        out = NA,
        bottom = bottom,
        top = top)
    } else {
      out_list[[i]] <- list(
        out = lm(form, dat, ...),
        bottom = bottom,
        top = top
      )
    }
  }
  out_list
}



make_data <- function(form, data) {
  if(!inherits(form, "formula")) {
    form <- as.formula(form)
  }
  mat <- model.matrix(form, data)
  y <- data[[getResponse(form)]]
}

#' Get response variable from formula
#' @param formula formula to be used
getResponse <- function(formula) {
  tt <- terms(formula)
  vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
  response <- attr(tt, "response") # index of response var
  vars[response]
}

#' Apply rolling function over a data.frame
#'
rollsmooth <- function(data, x, h) {

}

weights <- function(kern, x) {

}


