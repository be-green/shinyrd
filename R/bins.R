#' Guess binwidth for histogram
#' @importFrom stats IQR
#' @param x vector to use in histogram
#' @param method method to use for automatic binning if bw is null
#' @details The method guesses the bandwidth to be used as a default
#' in case the user doesn't specify their own parameter.
#' Options currently supported are "mccrary" and "freedman".
#' for details see [bw_freedman] and [bw_mccrary].
#' @export
guess_bw <- function(x, method = c("mccrary", "freedman")) {
  do.call(paste0("bw_", method), args = list(x = x))
}

#' McCrary Bandwidth Estimator for Histogram Bins
#' @importFrom stats sd
#' @param x vector to bin
#' @references McCrary, Justin. "Manipulation of the running variable in the
#' regression discontinuity design: A density test."
#' Journal of econometrics 142.2 (2008): 698-714.
#' @export
bw_mccrary <- function(x) {
  2 * stats::sd(x) / sqrt(length(x))
}

#' Freedman-Diaconis Histogram Binning Rule
#' @param x vector to bin
#' @references Freedman, David, and Persi Diaconis. "On the histogram as a density estimator:
#' \ifelse{html}{\out{L<sub>2</sub>} theory."
#' Zeitschrift fur Wahrscheinlichkeitstheorie und verwandte Gebiete 57.4 (1981): 453-476.
#' @export
bw_freedman <- function(x) {
  2 * stats::IQR(x) / length(x)^(1/3)
}

#' Bin a variable
#' @param rv running variable to put in bins
#' @param bw bandwidth to use for binning procedure
#' @param method method to use for automatic binning if bw is null
#' @importFrom assertthat assert_that
#' @importFrom data.table data.table
#' @details If bw (the bandwidth parameter) is null,
#' rv (the running variable) will be placed into bins based on
#' the automated procedure determined by the method argument.
#' Options currently supported are "mccrary" and "freedman".
#' for details see [bw_freedman] and [bw_mccrary].
#' @export
bin <- function(rv, bw = NULL, method = "mccrary") {
  assertthat::assert_that(is.numeric(rv))

  if(is.null(bw)) {
    bw <- guess_bw(rv, method = method)
  } else {
    assertthat::assert_that(is.numeric(bw))
  }

  bins <- seq(min(rv), max(rv), by = bw)

  med <- vector()
  for(i in 2:length(bins)) {
    med[i - 1] <- median(c(bins[i - 1], bins[i]))
  }

  freq <- tabulate(findInterval(rv, bins, all.inside = T))


  data.table::data.table(bin_median = med, freq = freq)[freq > 0]
}

#' Return Binned, Grouped Histogram for running variable,
#' above and below cutoff
#' @param rv running variable
#' @param bw bin width, defaults to automatic rule specified by method
#' @param normalize whether to normalize frequencies by the length of the vector
#' @param method method for determining automatic bin width, defaults to "mccrary"
#' @param cutoff cutoff to be used in discontinuity
#' @importFrom data.table rbindlist
#' @importFrom data.table setnames
#' @details This function takes in a running variable, a vector of cutoff points,
#' an optional bandwidth, and an optional method for automated binning procedure.
#' It returns a data.table that has gone through the first stage of the McCrary density test,
#' e.g. one with normalized count data for the median of each bin. Bins without
#' any observations are dropped. For details on the default binning procedure, see
#' [bin], [guess_bw], [bw_mccrary], and [bw_freedman].
#' @export
make_grid <- function(rv, cutoff, bw = NULL, normalize = T, method = "mccrary") {
  binned_groups <- list()

  cutoff <- sort(cutoff)

  for(i in 1:(length(cutoff))) {
    binned_groups[[paste0("<", cutoff[i])]] <- bin(rv = subset(rv, rv < cutoff[i]), bw = bw, method = method)
  }

  binned_groups[[paste0(">=", max(cutoff))]] <- bin(rv = subset(rv, rv >= max(cutoff)), bw = bw, method = method)

  binned_groups <- data.table::rbindlist(binned_groups, idcol = T, use.names = T)

  data.table::setnames(binned_groups, ".id", "group")

  if(normalize) {
    binned_groups[,freq := freq/length(freq)]
  }

  binned_groups[]
}

