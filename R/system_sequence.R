
# sequence generation ####

#' Define rules for calculating the Collatz sequence (with shortcut for odd numbers)
#' @param n value for step `n`
#' @return value for step `n + 1`
#' @references https://en.wikipedia.org/wiki/Collatz_conjecture
#' @export

collatz <- function(n) {

  if (n == 1) {
    purrr::done(n)
  } else {
    dplyr::case_when(
      n %% 2 == 0 ~ n/2,
      n %% 2 != 0 ~ (3*n + 1) / 2
    )
  }

}

#' Calculate the Collatz sequence recursively
#' @param i initial value
#' @param max maximum number of iterations
#' @return a numeric vector
#' @export
seq_collatz <- function(i, max = 1000) {
  purrr::accumulate(1:max, ~ collatz(.), .init = i) %>% utils::head(., -1)
}

#' Calculate a sequence of alternate values sampled from a Normal distribution
#' @param n sequence length
#' @param m mean value of the Normal distribution
#' @param sd standard deviation of the Normal distribution
#' @return a numeric vector of length n
#' @export
seq_alt <- function(n, m=60, sd=10) {stats::rnorm(n, m, sd)*(-1)^(1:n)}


# geometry ####

#' Recursively transform a vector as a function of length and angle values
#' @param .x initial or n-1 coordinate vector
#' @param .y n coordinate vector
#' @return A vector the same length of .x with the same names as .x
#' @export
transform_vector <- function(.x, .y) {
  .y %>% dplyr::mutate(
    x = .x$xend,
    y = .x$yend,
    xend = x + .y$length * cos((90 - .y$angle) * pi/180),
    yend = y + .y$length * sin((90 - .y$angle) * pi/180)
  )
}

#' Renders a 2D path with different methods
#' @param data a dataframe with x and y coordinates
#' @param scale scaling factor of the output
#' @param width a numeric vector for x and y shifts of the path
#' @param method method used to render the path. "spline" fits a b-spline to
#'   smooth the initial path. "path" shift the initial path according to `width`
#'   argument. "polygon" builds an oriented polygon. "polygon_lm" builds an
#'   oriented polygon with decreasing size along path length.
#' @return a dataframe with new x and y coordinates
#' @export
#'
transform_path <- function(data, scale = 1, width = c(0,10), method = "polygon") {

  switch(
    method,

    spline = {
      plot <- data %>% ggplot2::ggplot(ggplot2::aes(x, y)) + ggforce::geom_bspline()

      path <- ggplot2::layer_data(plot) %>%
        dplyr::select(x,y) %>%
        dplyr::mutate(xend = dplyr::lead(x), yend = dplyr::lead(y)) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(x = x + width[1], y = y + width[2]) %>%
        dplyr::summarise(
          x = c(x, rev(xend), x[1]),
          y = c(y, rev(yend), y[1] - width[2]))
    },

    path = {
      path <- data %>%
        dplyr::mutate(x = x, yend = yend + width[2])
    },

    polygon = {
      path <- data %>%
        dplyr::mutate(x = x + width[1], y = y + width[2]) %>%
        dplyr::summarise(
          x = c(x, rev(xend), x[1]),
          y = c(y, rev(yend), y[1] - width[2]))
    },

    polygon_lm = {
      path <- data %>%
        dplyr::mutate(
          l = scales::rescale(n, to = c(1,0)),
          x = x + width[1], y = y + width[2] * l) %>%
        dplyr::summarise(
          x = c(x, rev(xend), x[1]),
          y = c(y, rev(yend), y[1] - width[2]))
    }
  )

  return(path %>% dplyr::mutate(dplyr::across(c(x,y), ~ . * scale)))

}

# objects creation ####

#' Generate a path suitable to represent a leaf-like structure
#' @param i starting value for the sequence calculation
#' @param a value of the angle between leaf segments (degrees)
#' @param x0,y0 coordinates of the first leaf segments
#' @param shape method for the calculation of successive angles between leaf
#'   segments (character). "spiral" accumulates angle in the same direction.
#'   "wave" accumulates angle depending on the parity of the value in the
#'   sequence.
#' @return a dataframe with coordinates of leaf segments
#' @export
#'
gen_leaf <- function(i, a = 20, x0 = 0, y0 = 0, shape = "spiral") {

  # set parameters and initial value
  init <- tibble::tibble(s = seq_collatz(i)) %>%
    dplyr::mutate(
      n = seq_along(s),
      length = s,
      angle = dplyr::case_when(
        shape == "spiral" ~ n * a,
        shape == "wave" ~ cumsum(dplyr::if_else(s %% 2 == 0, a, -a))
      ),
      x = x0, y = y0,
      xend = x + i * cos((90 - a) * pi/180),
      yend = y + i * sin((90 - a) * pi/180)
    )

  # transform vector coordinates
  init %>%
    dplyr::group_by(n) %>% tidyr::nest() %>% dplyr::ungroup() %>%
    dplyr::mutate(
      data = purrr::accumulate(data, transform_vector)
    ) %>% tidyr::unnest(data)
}

#' Generate a collection of path suitable to represent a node-like structure
#' @param n number of path to simulate
#' @param imin,imax bounds of uniform distribution of the sequence starting
#'   value (int)
#' @param amin,amax bounds of uniform distribution of the angle between path
#'   segments (degreee)
#' @param lmax maximum value for simulated path length
#' @param shift vertical shift between paths
#' @param width a numeric vector for x and y shifts of the individual paths
#' @param scale scaling value applied on the complete node
#' @param shape method for the calculation of successive angles between leaf
#'   elements (character). "spiral" accumulates angle in the same direction.
#'   "wave" accumulates angle depending on the parity of the value in the
#'   sequence.
#' @param method method used to render the path. "spline" fits a b-spline to
#'   smooth the initial path. "path" shift the initial path according to `width`
#'   argument. "polygon" builds an oriented polygon. "polygon_lm" builds an
#'   oriented polygon with decreasing size along path length.
#' @param seed value of the random seed, random if missing
#' @return a dataframe with coordinates of multiple leafs
#' @export
#'
gen_node <- function(
  n = 20, imin = 20, imax = 70, lmax = 1000,
  amin = -20, amax = 20, shift = 20,
  width = c(0, 15), scale = 1, shape = "spiral", method = "polygon", seed) {

  # set seed if needed
  if (!missing(seed)) set.seed(seed)

  data <- tibble::tibble(
    id = seq_len(n),
    i = stats::runif(n, imin, imax) %>% as.integer(),
    a = stats::runif(n, amin, amax)) %>%
    dplyr::mutate(
      path = purrr::map2(i, a, ~ gen_leaf(i = .x, a = .y, shape = shape)),
      c_n = purrr::map_int(path, ~ nrow(.)),
      c_l = purrr::map_dbl(path, ~ sum(.$length))
    ) %>%
    dplyr::filter(c_l < lmax, a != 0) %>% tidyr::unnest(path)

  # shift organs vertically
  layout <- data %>%
    dplyr::left_join(
      data %>%
        dplyr::distinct(id, c_l) %>% dplyr::arrange(-c_l) %>%
        dplyr::mutate(shift = 0:(dplyr::n() - 1) * shift)
    ) %>%
    dplyr::mutate(dplyr::across(c(y, yend), ~ . + shift))

  # trace path with rendering function
  layout <- layout %>%
    dplyr::group_by(id, c_n, c_l, a) %>% tidyr::nest() %>%
    dplyr::mutate(
      path = purrr::map(
        data,
        ~ transform_path(., scale = scale, width = width, method = method)
        )) %>%
    dplyr::select(-data) %>% tidyr::unnest(path)

  return(layout)
}


# object rendering ####

#' Render nodes with different aesthetic
#' @param data a dataframe of objects coordinates produced by `gen_node()`
#'   function
#' @param method method used to render objects. "spline" render paths using
#'   B-splines. "segment" render leaf as a polygon with border and visible
#'   segments. "polygon" render closed path using `geom_shape()`.
#' @param radius radius of polygon smoothing
#' @param xlim,ylim limits passed to `coord_fixed()`
#' @param margin margins passed to `theme()`
#' @return a ggplot object
#' @export

render_node <- function(
  data, method = "polygon", radius = 0,
  xlim=NULL, ylim = NULL, margin = 0) {

  switch(
    method,

    path = {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(x, y, group = id)) +
        ggplot2::geom_path(size = 0.5, alpha = 1)
    },

    spline = {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(x, y, group = id)) +
        ggforce::geom_bspline(size = 0.5, alpha = 1)
    },

    segment = {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(x, y, xend = xend, yend = yend, group = id)) +
        ggplot2::geom_segment(size = 0.5, lineend = "round", alpha = 0.3) +
        ggplot2::geom_path(size = 0.5, alpha = 1) +
        ggplot2::geom_path(ggplot2::aes(x=xend, y=yend), size=0.5, alpha = 1)
    },

    polygon = {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(x,y, group = id)) +
        ggforce::geom_shape(
          color="black", fill="white",
          size = 0.5, radius = grid::unit(radius, 'pt'))
    },

    sf = {}
  )

  return(
    plot +
      ggplot2::coord_fixed(xlim = xlim, ylim = ylim) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin=rep(grid::unit(margin,"pt"),4))
  )

}

