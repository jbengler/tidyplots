% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-misc.R
\name{add_density_curve}
\alias{add_density_curve}
\title{Add density curve}
\usage{
add_density_curve(
  plot,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  n = 512,
  color = "#E37D46",
  fill = "#E37D46",
  alpha = 0.3,
  ...
)
}
\arguments{
\item{plot}{A \code{tidyplot} generated with the function \code{tidyplot()}.}

\item{bw}{The smoothing bandwidth to be used.
If numeric, the standard deviation of the smoothing kernel.
If character, a rule to choose the bandwidth, as listed in
\code{\link[stats:bandwidth]{stats::bw.nrd()}}. Note that automatic calculation of the bandwidth does
not take weights into account.}

\item{adjust}{A multiplicate bandwidth adjustment. This makes it possible
to adjust the bandwidth while still using the a bandwidth estimator.
For example, \code{adjust = 1/2} means use half of the default bandwidth.}

\item{kernel}{Kernel. See list of available kernels in \code{\link[=density]{density()}}.}

\item{n}{number of equally spaced points at which the density is to be
estimated, should be a power of two, see \code{\link[=density]{density()}} for
details}

\item{color}{common}

\item{fill}{common}

\item{alpha}{common}

\item{...}{Arguments passed on to the \code{geom} function.}
}
\value{
A \code{tidyplot} object
}
\description{
Add density curve
}
