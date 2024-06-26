% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-misc.R
\name{add_boxplot}
\alias{add_boxplot}
\title{Add boxplot}
\usage{
add_boxplot(
  plot,
  dodge_width = NULL,
  saturation = 0.3,
  show_whiskers = TRUE,
  show_outliers = FALSE,
  box_width = 0.6,
  whiskers_width = 0.5,
  outlier.size = 0.5,
  coef = 1.5,
  outlier.shape = 19,
  linewidth = 0.25,
  preserve = "total",
  ...
)
}
\arguments{
\item{plot}{A \code{tidyplot} generated with the function \code{tidyplot()}.}

\item{dodge_width}{For adjusting the distance between grouped objects.}

\item{saturation}{common}

\item{show_whiskers}{bla}

\item{show_outliers}{bla}

\item{box_width}{bla}

\item{whiskers_width}{bla}

\item{outlier.size}{bla}

\item{coef}{Length of the whiskers as multiple of IQR. Defaults to 1.5.}

\item{outlier.shape}{bla}

\item{linewidth}{Thickness of the line in points (pt). Typical values range between \code{0.25} and \code{1}.}

\item{preserve}{Should dodging preserve the \code{"total"} width of all elements at
a position, or the width of a \code{"single"} element?}

\item{...}{Arguments passed on to the \code{geom} function.}
}
\value{
A \code{tidyplot} object
}
\description{
Add boxplot
}
