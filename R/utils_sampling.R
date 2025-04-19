# function for sampling in 1D or 2D
# TODO write functions for space-filling designs in 2D: gaussian, uniform, LHS, noise.

# 1D ####

#' Modify the base jitter() function to return the original vector if the jitter amount is zero
#' @param x, numeric vector to which jitter should be added.
#' @param amount, jitter quantity if positive, else returns the original vector (numeric)
#' @return a noisy numeric vector
#' @export
add_noise <- function(x, amount = NULL) {
  if (amount > 0) jitter(x, amount = amount) else x
}


# https://en.wikipedia.org/wiki/Low-discrepancy_sequence

#' Create a regular sequence of discrete numbers with noise
#' @param start,end bounds for the generated sequence
#' @param n sequence length
#' @param jitter bounds for the noise distribution
#' @return a vector of integers
#' @export
seq_noise <- function(start, end, n, jitter = 1) {

  # generate regularly spaced numbers
  samples <- seq(start, end, length.out = n)

  # generate uniform noise
  noise <- stats::runif(n, -jitter, jitter) |> round()

  # add noise
  out <- samples + noise

  return(out)
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


# 2D ####

#' Sample n points uniformly in a rectangular area
#' @param n number of points
#' @param method method used to sample the space
#' * "uniform": uniform distributions for each dimensions.
#' * "Halton": Halton low discrepancy sequence from `randtoolbox::halton()`.
#' @param xlim,ylim x and y limits, default to (-1,1)
#' @return a dataframe with n, x, and y columns
#' @export
#'
layout_rectangle <- function(
  n = 7, method = "uniform",
  xlim = c(-1,1), ylim = c(-1,1)) {

  switch(
    method,

    uniform = {
      tibble::tibble(
        n=1:n,
        x=stats::runif(n, xlim[1], xlim[2]),
        y=stats::runif(n, ylim[1], ylim[2])
      )
    },

    halton = {
      randtoolbox::halton(n, dim = 2) |>
        magrittr::set_colnames(c("x","y")) |> dplyr::as_tibble() |>
        dplyr::mutate(
          n = seq_along(x),
          x = scales::rescale(x, to = xlim),
          y = scales::rescale(y, to = ylim)
        )
    },

    stop("Invalid `method` value")

  )

}

#' Sample n points in an elliptic area with different spatial repartition.
#' @param method
#' * uniform: Points are sampled from uniform distribution in polar coordinates. The point set is then transformed to cartesian coordinates, scaled, and rotated.
#' * gaussian: Points are sampled from a bivariate Gaussian (μ = 0, sd = r)
#' * spiral: Points are arranged using Fermat's spiral ([Vogel, 1979](https://doi.org/10.1016%2F0025-5564%2879%2990080-4)) (a = pi x (1 + sqrt(5)))
#' @param n number of points
#' @param x0,y0 coordinates of the center of the sampling area
#' @param r radius of the sampling area
#' @param s variance for gaussian radius of the sampling area
#' @param a rotation angle of the point set (radians)
#' @param scale_x scaling coefficient applied on the x axis
#' @return a dataframe with n, x, and y columns
#' @export

layout_ellipse <- function(
    n = 7, x0 = 0, y0 = 0, r = 1, s = 0, a = -pi/6, scale_x = 0.5, method = "uniform"
  ) {

  switch (
    method,

    uniform = {

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
    },

    gaussian = {

      layout <- MASS::mvrnorm(
        n = n, mu = c(x0,y0),
        Sigma = matrix(c(r, 0, 0, r), ncol = 2)) |>
        magrittr::set_colnames(c("x","y")) |>
        dplyr::as_tibble(.name_repair = "unique") |>
        dplyr::mutate(n = 1:n()) |> dplyr::select(n, x, y)
    },

    # phi = pi * (1 + sqrt(5))
    spiral = {

      layout <- tibble::tibble(
        n = 0:(n-1),
        r = scales::rescale(sqrt(n/100), to = c(0,r)),
        theta = a * n,
        x = x0 + r * cos(theta),
        y = y0 + r * sin(theta)
      ) |> dplyr::select(n, x, y)
    },

    stop("Invalid `method` value")

  )



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

#' Sample n point regularly spaced on a rectangle
#' @param n number of points
#' @param scaling expansion factor
#' @param jitter jitter factor
#' @param x0,y0,x,y coordinates of the center, width and height of the sampled rectangle
#' @return a dataframe with x and y columns
#' @export
sample_rectangle <- function(
    n = 200, scaling = 0, jitter = 10,
    x0, y0, x, y) {

  # apply scaling factor
  w <- x + scaling * x
  h <- y + scaling * x

  frame <- tibble::tibble(
    i = 0:(n-1),
    t = utils::head(seq(0, 2 * pi, by = (2 * pi)/length(i)), -1),
    x = w/2 * (abs(cos(t)) * cos(t) + abs(sin(t)) * sin(t)) + x0,
    y = h/2 * (abs(cos(t)) * cos(t) - abs(sin(t)) * sin(t)) + y0) |>
    dplyr::mutate(x = jitter(x, jitter), y = jitter(y, jitter))

  return(frame)

}


#' Sample a rectangular area defined by its relative size and aspect ratio nested in a larger area.
#' @param xlim,ylim limits of the larger sampling area
#' @param r aspect ratio of the small sampled area
#' @param p proportion of the small sampled area (relative length)
#' @export
sample_box <- function(xlim = c(-1, 1), ylim = c(-1, 1), r = 0.7, p = 0.2) {

  # define box dimensions
  if (r > 1) {
    h <- p * abs(ylim[2] - ylim[1])
    w <- h * r
  } else {
    w <- p * abs(xlim[2] - xlim[1])
    h <- w / r
  }

  # sample a random middle point within a safe range
  center <- c(stats::runif(1, xlim[1] + (w / 2), xlim[2] - (w / 2)),
              stats::runif(1, ylim[1] + (h / 2), ylim[2] - (h / 2)))

  # calculate the limits, ensuring they stay within the sampling area
  box_xlim <- c(center[1] - w / 2, center[1] + w / 2)
  box_ylim <- c(center[2] - h / 2, center[2] + h / 2)

  return(list(xlim = box_xlim, ylim = box_ylim))
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


