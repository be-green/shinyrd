#' List of supported kernels
#' @details List of supported kernel weighting schemes
#' @export
kerns <- c(
  "gaussian",
  "rectangular",
  "triangular",
  "epanechnikov",
  "biweight",
  "cosine",
  "optcosine"
)

#' Available kernel weighting functions
#' @param x vector to weight
#' @param bw bandwidth to use
#' @inheritParams kernels
#' @rdname kernels
#' @export
weight_gaussian <- function(x, bw)  {
  dnorm(x, sd = bw)
}

#' @inheritParams kernels
#' @rdname kernels
#' @export
weight_rectangular <- function(x, bw) {
  a <- bw * sqrt(3)
  ifelse(abs(x) < a, 0.5/a, 0)
}

#' @inheritParams kernels
#' @rdname kernels
#' @export
weight_triangular <- function(x, bw) {
  a <- bw * sqrt(6)
  ax <- abs(x)
  ifelse(ax < a, (1 - ax/a)/a, 0)
}

#' @inheritParams kernels
#' @rdname kernels
#' @export
weight_epanechnikov <- function(x, bw) {
  a <- bw * sqrt(5)
  ax <- abs(x)
  ifelse(ax < a, 3/4 * (1 - (ax/a)^2)/a, 0)
}

#' @inheritParams kernels
#' @rdname kernels
#' @export
weight_biweight <- function(x, bw) {
  a <- bw * sqrt(7)
  ax <- abs(x)
  ifelse(ax < a, 15/16 * (1 - (ax/a)^2)^2/a, 0)
}

#' @inheritParams kernels
#' @rdname kernels
#' @export
weight_cosine <- function(x, bw) {
  a <- bw/sqrt(1/3 - 2/pi^2)
  ifelse(abs(x) < a, (1 + cos(pi * x/a))/(2 * a), 0)
}

#' @inheritParams kernels
#' @rdname kernels
#' @export
weight_optcosine <- function(x, bw) {
  a <- bw/sqrt(1 - 8/pi^2)
  ifelse(abs(x) < a, pi/4 * cos(pi * x/(2 * a))/a, 0)
}

#' Stop if kernel is not supported
stop_not_kernel <- function() {
  stop("Kernel must be one of: \n\t",
       paste0(kerns, collapse = ",\n\t"))
}

#' Weight according to kernel function
#' @param x vector to weight according to kernel function
#' @param bw bandwidth to use for weighting
#' @param kernel kernel weighting function to use
#' @export
weight <- function(x, bw, kernel) {
  if(!kernel %in% kerns) {
    stop_not_kernel()
  } else {
    do.call(paste0("weight_",kernel),
            args = list(
              x = x,
              bw = bw
            ))
  }
}
