% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system_ode.R
\name{simulate_duffing}
\alias{simulate_duffing}
\title{Simulator for the Duffing dynamical system}
\usage{
simulate_duffing(
  parameters,
  start_coords = c(x = 0, y = 0),
  t_max = 10,
  t_step = 1/100
)
}
\arguments{
\item{parameters}{a,b,c describe the amount of damping, the amplitude of the periodic driving force, and the angular frequency of the periodic driving force.}

\item{start_coords}{initial conditions (numeric vector)}

\item{t_max, t_step}{end time and step of the simulation (numeric).}
}
\value{
a dataframe with time, x, and y columns
}
\description{
Describes certain damped and driven oscillators. Coded from http://paulbourke.net/fractals/duffing/.
}
