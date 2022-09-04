# function for sampling in 1D or 2D
# TODO write functions for space-filling designs in 2D: gaussian, uniform, LHS, noise.

# 1D ####
# https://en.wikipedia.org/wiki/Low-discrepancy_sequence

# 2D ####

#' Sample n points uniformly in a square
#' @param n number of points
#' @param method method used to sample the space
#' * "uniform": basic sampling based on independent uniform distributions for each dimensions.
#' * "sobol": sampling using uniform Sobol low discrepancy sequences from `randtoolbox::sobol()`.
#' @param xlim,ylim x and y limits, default to (-1,1)
#' @param seed seed used for the sobol method.
#' @return a dataframe with n, x, and y columns
#' @export
#'
layout_square <- function(
  n = 7, method = "uniform",
  xlim = c(-1,1), ylim = c(-1,1),
  seed) {

  switch(
    method,

    uniform = {
      tibble::tibble(
        n=1:n,
        x=stats::runif(n, xlim[1], xlim[2]),
        y=stats::runif(n, ylim[1], ylim[2])
      )
    },

    sobol = {
      xlim[1] + (xlim[2] - xlim[1]) *
        randtoolbox::sobol(n, dim = 2, scrambling = 1, seed = seed) |>
        tibble::as_tibble() |>
        dplyr::mutate(n = seq_along(V1)) |>
        dplyr::select(n, x=V1, y=V2)
    },

    stop("Invalid `method` value")

  )

}

#' Sample n points in an elliptic area
#' @description Point are sampled from uniform distribution in polar coordinates. The point set is then transformed to cartesian coordinates, scaled, and rotated.
#' @param n number of points
#' @param x0,y0 coordinates of the center of the sampling area
#' @param r radius of the sampling area
#' @param a rotation angle of the point set (radians)
#' @param scale_x scaling coefficient applied on the x axis
#' @return a dataframe with n, x, and y columns
#' @export

layout_ellipse <- function(
    n = 7, x0 = 0, y0 = 0, r = 1, a = -pi/6, scale_x = 0.5
  ) {
  r = sqrt(stats::runif(n, 0, r))
  theta = stats::runif(n, 0, 2*pi)

  # scale and rotate layout
  layout <- tibble::tibble(
    n = 1:n,
    x0 = x0 + r * cos(theta),
    y0 = y0 + r * sin(theta)) |>
    dplyr::mutate(x0 = x0 * scale_x) |>
    dplyr::mutate(
      x = x0*cos(a) - y0*sin(a),
      y = x0*sin(a) + y0*cos(a)) |>
    dplyr::select(n,x,y)

  return(layout)
}

#' Sample n point regularly spaced on a circle
#' @param n number of points
#' @param x0,y0 coordinates of the center of the sampled circle
#' @param r radius of the sampled circle
#' @return a dataframe with n, x, and y columns
#' @export

sample_perimeter <- function(n = 7, x0 = 0, y0 = 0, r = 1) {

  tibble::tibble(
    n = 0:(n-1),
    theta = utils::head(seq(0, 2*pi, by = (2*pi)/length(n)), -1),
    x = x0 + r * cos(theta),
    y = y0 + r * sin(theta)
  )

}

#' Sample n points evenly distributed on a disc
#' @description Points are arranged using Fermat's spiral ([Vogel, 1979](https://doi.org/10.1016%2F0025-5564%2879%2990080-4))
#' @param n number of points
#' @param x0,y0 coordinates of the center of the sampled circle
#' @param r radius of the sampled circle
#' @param a angle in polar coordinates (radians)
#' @return a dataframe with n, x, and y columns
#' @export

sample_disc <- function(n = 100, x0 = 0, y0 = 0, r = 1, a = pi * (1 + sqrt(5))) {

  tibble::tibble(
    n = 0:(n-1),
    r = scales::rescale(sqrt(n/100), to = c(0,r)),
    theta = a * n,
    x = x0 + r * cos(theta),
    y = y0 + r * sin(theta)
  )

}

#' Sample and replace values with NA in a vector
#' @param x,y input vectors
#' @param p proportion of missing values (0,1)
#' @param method method from sampling values
#' @return a vector with missing values.
#' @export

sample_missing <- function(x, y = NULL, p, method = "random") {

  n <- length(x)

  switch(
    method,

    random = {
      s <- sample(1:n, size = p * n)
      x[s] <- NA
      return(x)
    },

    along = {
      #y <- scales::rescale(y, to = c(1, n))
      s <- truncnorm::rtruncnorm(
        n = p * n, a = 0, b = n,
        mean = mean(y), sd = stats::sd(y))
      #s <- scales::rescale(s, to = c(1, n))
      x[s] <- NA
      return(x)
    },

    stop("Invalid `method` value")
  )
}

#' Sample equidistant points on the perimeter of l concentric circles of r radius
#' @param x0,y0 coordinates of the center of the concentric circles
#' @param l number of concentric circles
#' @param r radius of the outermost circle
#' @param jitter amount of jitter to apply to sampled points
#' @export
#'
sample_vessel <- function(x0 = 0, y0 = 0, l = 5, r = 1/100, jitter = 1/500) {
  tibble::tibble(l = 1:l) |>
    dplyr::mutate(n = l * 10, r = seq(1/40, r * dplyr::n(), len = dplyr::n())) |>
    dplyr::mutate(data = purrr::map2(n, r, ~ sample_perimeter(n = ..1, r = ..2))) |>
    dplyr::select(l, data) |> tidyr::unnest(data) |>
    dplyr::mutate(x = x + x0, y = y + y0) |>
    dplyr::mutate(dplyr::across(x:y, ~ jitter(., a = jitter)))
}


