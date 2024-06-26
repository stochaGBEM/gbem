% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eroding_flow.R
\name{eroding_flow}
\alias{eroding_flow}
\title{Flow Threshold for Channel Erosion}
\usage{
eroding_flow(cross_section)
}
\arguments{
\item{cross_section}{A \code{"cross_section"} object representing a stream's
cross section.}
}
\value{
Discharge; single numeric.
}
\description{
For a given channel, calculates the largest flow a channel can accommodate
without eroding.
}
\examples{
cs <- cross_section(3, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
eroding_flow(cs)
}
