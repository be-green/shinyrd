% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smooths.R
\name{bandwidth_mccrary}
\alias{bandwidth_mccrary}
\title{Choose bandwidth for density test based on McCrary's Criterion}
\usage{
bandwidth_mccrary(rv, cutoff, method = "mccrary", ...)
}
\arguments{
\item{rv}{running variable}

\item{cutoff}{cutoff(s) for discontinuity}

\item{method}{method for selecting bin width for first-step histogram smoother}

\item{...}{ignored for now}
}
\description{
Choose bandwidth for density test based on McCrary's Criterion
}
\details{
The procedure as detailed in McCrary (2008) recommends
using an automated process as a starting point and subsequently
implementing a subjective rule, while reporting the difference
from the automated process. For details, please see the paper.

For details on the first-step histogram smoother, see \link{make_grid}
or \link{bin}.
}
\references{
McCrary, Justin. "Manipulation of the running variable in the
regression discontinuity design: A density test."
Journal of econometrics 142.2 (2008): 698-714.
}
