% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_sampling.R
\name{add_noise}
\alias{add_noise}
\title{Modify the base jitter() function to return the original vector if the jitter amount is zero}
\usage{
add_noise(x, amount = NULL)
}
\arguments{
\item{x, }{numeric vector to which jitter should be added.}

\item{amount, }{jitter quantity if positive, else returns the original vector (numeric)}
}
\value{
a noisy numeric vector
}
\description{
Modify the base jitter() function to return the original vector if the jitter amount is zero
}
