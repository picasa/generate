% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_geometry.R
\name{tr_jitter}
\alias{tr_jitter}
\title{Add jitter to a 2D object}
\usage{
tr_jitter(data, a, index = NULL)
}
\arguments{
\item{data}{dataframe with x and y coordinates, and id columns}

\item{a}{jitter amount (units)}

\item{index}{string for the index column to be added from the original data}
}
\value{
a dataframe with new coordinates columns
}
\description{
Add jitter to a 2D object
}
