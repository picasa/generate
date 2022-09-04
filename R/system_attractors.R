# model and simulation ####

#' Difference equation for the logistic model
#' @param p parameter vector
#' @param xn,yn x and y value for step n
#' @return a vector of length two with n+1 values
#' @export
logistic_map <- function(p, xn, yn) {
  xn1 <- p[1] * xn * (1 - xn)
  yn1 <- 0

  c(xn1, yn1)
}

#' Difference equations for quadratic model
#' @param p parameter vector
#' @param xn,yn x and y value for step n
#' @return a vector of length two with n+1 values
#' @references https://blog.k2h.se/post/hunting-for-attractors/
#' @export
#'
quadratic_map <- function(p, xn, yn) {
  xn1 <- p[1] + p[2]*xn + p[3]*xn*xn + p[ 4]*xn*yn + p[ 5]*yn + p[ 6]*yn*yn
  yn1 <- p[7] + p[8]*xn + p[9]*xn*xn + p[10]*xn*yn + p[11]*yn + p[12]*yn*yn

  c(xn1, yn1)
}

#' Difference equation for the Henon model
#' @param p parameter vector
#' @param xn,yn x and y value for step n
#' @return a vector of length two with n+1 values
#' @references http://www.complexification.net/gallery/machines/henonPhase/
#' @export
henon_phase <- function(p, xn, yn) {
  xn1 <- xn * cos(p[1]) - (yn - xn^2) * sin(p[1])
  yn1 <- xn * sin(p[1]) + (yn - xn^2) * cos(p[1])

  c(xn1, yn1)
}

#' Iterate difference equations
#' @param f function to iterate
#' @param p parameter vector
#' @param x0,y0 initial values for x and y
#' @param iterations number of iterations
#' @param ... used for mapping
#' @return a dataframe with x, y coordinates, and iteration number.
#' @references https://blog.k2h.se/post/hunting-for-attractors/
#' @export
iterate <- function(f, p, x0, y0, iterations, ...) {
  x <- rep(x0, iterations)
  y <- rep(y0, iterations)

  for(n in 1:(iterations - 1)) {
    xy <- f(p, x[n], y[n])
    x[n+1] <- xy[1]
    y[n+1] <- xy[2]
  }

  tibble::tibble(x = x, y = y) |>
    dplyr::mutate(iteration = dplyr::row_number())
}


#' Compute the Lyapunov exponent to estimate occurrence of a chaotic behaviour
#' @param f function to iterate
#' @param p parameter vector
#' @param x0,y0 initial values for x and y
#' @param iterations number of iterations
#' @return The value of the Lyapunov exponent
#' @references https://blog.k2h.se/post/hunting-for-attractors/
#' @export

L <- function(f, p, x0, y0, iterations = 1000) {
  # put the point nearby and see what happens
  nearby_distance <- 0.000001

  xy <- c(x0, y0)
  xy_near <- xy + c(nearby_distance, 0)

  # Collect the log distance ratios as we iterate
  sum_log_distance_ratios <- 0

  for (n in 1:iterations) {
    xy <- f(p, xy[1], xy[2])
    xy_near <- f(p, xy_near[1], xy_near[2])

    new_distance = sqrt((xy[1] - xy_near[1])^2 + (xy[2] - xy_near[2])^2)

    if (new_distance == 0) {
      # The points have converged
      return (-Inf)
    }

    if (abs(new_distance) == Inf) {
      # The points have run away
      return (Inf)
    }

    # Move near point after xy
    # We put the near point just behind in the direction that they differ
    angle = atan2(xy_near[2] - xy[2], xy_near[1] - xy[1])
    xy_near <- c(xy[1] + nearby_distance * cos(angle),
                 xy[2] + nearby_distance * sin(angle))


    sum_log_distance_ratios = sum_log_distance_ratios + log2(new_distance / nearby_distance)

  }

  sum_log_distance_ratios / iterations
}

#' Normalize xy coordinates
#' @param data dataframe with x and y coordinates
#' @param n number of starting iterations to remove
#' @return a normalized dataframe with x and y coordinates
#' @export

normalize_xy <- function(data, n = 100) {

  # remove outliers (usually in first iterations)
  data <- data |> dplyr::slice(n = -(1:n))

  range <- with(data, max(max(x) - min(x), max(y) - min(y)))

  data |>
    dplyr::mutate(
      x = (x - min(x)) / range,
      y = (y - min(y)) / range
    )
}

#' Bin xy coordinates to reduce resolution
#' @param data dataframe with x and y coordinates
#' @param gridsize size of the grid (larger is more precise)
#' @return a downsampled dataframe with x and y coordinates
#' @export

bin_xy <- function(data, gridsize = 20) {

   data |>
    dplyr::group_by(x = round(x * gridsize) / gridsize,
             y = round(y * gridsize) / gridsize) |>
    dplyr::summarize(n = dplyr::n()) |> dplyr::ungroup()

}


# sampling ####

#' Random sampling of character sequence
#' @param set Letter set to use
#' @param length Length of the output sequence
#' @return a random character sequence of given length
#' @export

sample_sequence <- function(set = LETTERS[1:25], length = 12) {
  paste0(sample(set, size = length, replace=TRUE), collapse = "")
}

#' Discrete sampling of the parameter space using letter combinations.
#' @param string Letter string to decode to numerical values
#' @param scale Numerical scale used for mapping
#' @return a numeric vector
#' @references Sprott, J. C. (1993). Automatic generation of strange attractors.
#'   Computers & Graphics, 17(3), 325-332.
#' @export
get_parameters <- function(
  string="FIRCDERRPVLD",
  scale = seq(from = -1.2, to = 1.2, by = 0.1)) {

  names(scale) <- LETTERS[1:25]

  scale[stringr::str_split(string, "", simplify = TRUE)]

}

#' Summarize the density distribution of points per grid cell
#' @param data dataframe with x and y coordinates
#' @param gridsize size of the grid (larger is more precise)
#' @return a dataframe with the relative density (d), mean number of point in
#'   cell grid (m), and coefficient of variation (cv).
#' @export

density_metric <- function(data, gridsize = 20) {
  data |>
    normalize_xy() |>
    bin_xy(gridsize) |>
    dplyr::summarise(
      d = dplyr::n() / gridsize^2,
      m = mean(n) / sum(n) * 100,
      cv = stats::sd(n) / mean(n)
    )
}


# rendering ####

#' Render a single character/scatterplot
#' @param data dataframe with x and y coordinates
#' @param color Point color
#' @param size Point size
#' @param alpha Point alpha
#' @return a plot for a single point set
#' @export

render_plot <- function(data, color="black", size=0.5, alpha=1/10) {

  data |>
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_point(color = color, size = size, alpha = alpha) +
    ggplot2::coord_equal() + ggplot2::theme_void()
}

#' Sample and render a sequence of plots in a list
#' @param n size of the sample to draw
#' @param length size of the grid used to draw individual plots
#' @param data vector of plots to sample
#' @return a grid of plots
#' @export

render_sequence <- function(n, length, data) {
    sample(data, size = n, replace = TRUE) |>
    cowplot::plot_grid(plotlist = ., nrow = length) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = grid::unit(c(1,0,1,0), "cm"))
}

#' Render a grid of plot using a text sequence as pattern
#' @param text a character string
#' @param data a set of plot of characters
#' @param ncol number of columns of the grid of plots
#' @param scale scale of individual plots
#' @param align parameter for plot alignment (cowplot)
#' @return a grid of plots
#' @export

render_paragraph <- function(text, data, ncol = 80, scale = 0.8, align = "none") {
  seq <- text |> stringr::str_to_lower() |> stringr::str_split(., "")

  # join characters with glyph plots, non-match creates NULL plot (blank space with cowplot)
  data <- tibble::tibble(character=seq[[1]]) |>
    dplyr::left_join(data |> dplyr::select(character, pattern, plot = plot_ld))

  cowplot::plot_grid(plotlist = data$plot, ncol=ncol, scale=scale, align = align)
}

