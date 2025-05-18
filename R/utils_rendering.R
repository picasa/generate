
# rendering ####

#' Generate a paper texture based on small fibers
#' @param n texture density
#' @param jitter length of texture elements
#' @param strength curvature in texture elements
#' @param size,color linewidth and color of texture elements
#' @param expansion additive scaling for limits
#' @return a ggplot object
#' @export
gen_paper <- function(
    n = 30e3, jitter = 1/30, strength = 0.5,
    size = 1/10, color = "black", expansion = -1/50) {

  plot <- dplyr::tibble(
    x = stats::runif(n),
    y = stats::runif(n),
    xend = jitter(x, amount = jitter),
    yend = jitter(y, amount = jitter)
  ) |>
    ggplot2::ggplot(ggplot2::aes(x, y, xend = xend, yend = yend)) +
    ggforce::geom_diagonal0(
      strength = strength, linewidth = size,
      color = color, alpha = 1/20) +
    ggplot2::scale_x_continuous(expand = expansion(add = expansion)) +
    ggplot2::scale_y_continuous(expand = expansion(add = expansion)) +
    ggplot2::theme_void() + ggplot2::theme(legend.position = "none")

  return(plot)

}

# themes ####

#' Customize a theme object with margins, color, and background.
#' @param output string to select "digital" or "plotter" theme.
#' "plotter" theme strips out panel backgroung and borders for a clean svg output.
#' @param color background color for digital output, default to an oldish white color.
#' @param border,width border color and width, default to NA.
#' @param margin a vector of four numeric values for margins (top, right, bottom, left) in mm.
#' @return a ggplot theme object
#' @export
theme_paper <- function(
    output = "digital", color = "#FEFAEE",
    border = NA, width = NA, margin = rep(0,4)) {

  switch (
    output,
    digital = {
      theme <- ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(
          fill = color, linewidth = width, colour = border),
        plot.margin = grid::unit(margin, "mm")
      )
    },

    plotter = {
      theme <- ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        plot.margin = grid::unit(margin, "mm")
      )
    },

    stop("Invalid `output` value")
  )

  return(theme)

}
