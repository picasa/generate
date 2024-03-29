% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system_attractors.R
\name{get_parameters}
\alias{get_parameters}
\title{Discrete sampling of the parameter space using letter combinations.}
\usage{
get_parameters(
  string = "FIRCDERRPVLD",
  scale = seq(from = -1.2, to = 1.2, by = 0.1)
)
}
\arguments{
\item{string}{Letter string to decode to numerical values}

\item{scale}{Numerical scale used for mapping}
}
\value{
a numeric vector
}
\description{
Discrete sampling of the parameter space using letter combinations.
}
\references{
Sprott, J. C. (1993). Automatic generation of strange attractors.
Computers & Graphics, 17(3), 325-332.
}
