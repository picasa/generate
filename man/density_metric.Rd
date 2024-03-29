% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system_attractors.R
\name{density_metric}
\alias{density_metric}
\title{Summarize the density distribution of points per grid cell}
\usage{
density_metric(data, gridsize = 20)
}
\arguments{
\item{data}{dataframe with x and y coordinates}

\item{gridsize}{size of the grid (larger is more precise)}
}
\value{
a dataframe with the relative density (d), mean number of point in
cell grid (m), and coefficient of variation (cv).
}
\description{
Summarize the density distribution of points per grid cell
}
