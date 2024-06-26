% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-misc.R
\name{add_violin}
\alias{add_violin}
\title{Add violin plot}
\usage{
add_violin(
  plot,
  dodge_width = NULL,
  saturation = 0.3,
  draw_quantiles = NULL,
  trim = FALSE,
  linewidth = 0.25,
  scale = "width",
  ...
)
}
\arguments{
\item{plot}{A \code{tidyplot} generated with the function \code{tidyplot()}.}

\item{dodge_width}{For adjusting the distance between grouped objects.}

\item{saturation}{common}

\item{draw_quantiles}{If \code{not(NULL)} (default), draw horizontal lines
at the given quantiles of the density estimate.}

\item{trim}{If \code{TRUE} (default), trim the tails of the violins
to the range of the data. If \code{FALSE}, don't trim the tails.}

\item{linewidth}{Thickness of the line in points (pt). Typical values range between \code{0.25} and \code{1}.}

\item{scale}{if "area" (default), all violins have the same area (before trimming
the tails). If "count", areas are scaled proportionally to the number of
observations. If "width", all violins have the same maximum width.}

\item{...}{Arguments passed on to the \code{geom} function.}
}
\value{
A \code{tidyplot} object
}
\description{
Add violin plot
}
