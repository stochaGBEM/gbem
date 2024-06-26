% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.hydrograph.R
\name{plot.hydrograph}
\alias{plot.hydrograph}
\title{Plot a hydrograph}
\usage{
\method{plot}{hydrograph}(x, ..., ylab = "Discharge", xlab = "Time")
}
\arguments{
\item{x}{Hydrograph object.}

\item{...}{Other arguments to pass to the \code{curve()} function.}

\item{ylab, xlab}{Y-axis and x-axis labels for the plot.}
}
\value{
A base R plot of the hydrograph.
}
\description{
Plot a hydrograph
}
\details{
Calls the \code{curve()} function on the hydrograph function,
plotted from the beginning to the end of the hydrograph event timeline.
}
