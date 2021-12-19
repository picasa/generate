# function for sampling in 1D or 2D
# TODO write functions for space-filling designs in 2D: gaussian, uniform, LHS, noise.

# 1D ####

# 2D ####
# draw n points uniformly in in x and y
# https://en.wikipedia.org/wiki/Low-discrepancy_sequence

#' @export
#'
layout_square <- function(
  n = 7, method="uniform",
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
        randtoolbox::sobol(n, dim = 2, scrambling = 1, seed = seed) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(n = seq_along(V1)) %>%
        dplyr::select(n, x=V1, y=V2)
    }

  )

}

# draw n points in a unit circle
# https://mathworld.wolfram.com/DiskPointPicking.html
#' @export

layout_ellipse <- function(
  x0 = 0, y0 = 0, r = 1, a = -pi/6, n = 7, scale_x = 0.5
  ) {
  r = sqrt(stats::runif(n, 0, r))
  theta = stats::runif(n, 0, 2*pi)

  # scale and rotate layout
  layout <- tibble::tibble(
    n = 1:n,
    x0 = x0 + r * cos(theta),
    y0 = y0 + r * sin(theta)) %>%
    dplyr::mutate(x0 = x0 * scale_x) %>%
    dplyr::mutate(
      x = x0*cos(a) - y0*sin(a),
      y = x0*sin(a) + y0*cos(a)) %>%
    dplyr::select(n,x,y)

  return(layout)
}

# sample n point regularly spaced on a circle
#' @export

sample_perimeter <- function(x0 = 0, y0 = 0, r = 1, n = 7) {

  tibble::tibble(
    n = 0:(n-1),
    theta = utils::head(seq(0, 2*pi, by = (2*pi)/length(n)), -1),
    x = x0 + r * cos(theta),
    y = y0 + r * sin(theta)
  )

}

# sample n points regularly on a disc
#' @export

sample_disc <- function(x0 = 0, y0 = 0, r = 1, n = 100) {

  tibble::tibble(
    n = 0:(n-1),
    r = scales::rescale(sqrt(n/100), to = c(0,r)),
    theta = pi * (1 + sqrt(5)) * n,
    x = x0 + r * cos(theta),
    y = y0 + r * sin(theta)
  )

}

# sample and replace values with missing in a vector
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
    }
  )
}

