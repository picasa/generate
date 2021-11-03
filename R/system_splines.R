
# merge character layouts into words, drop failed mappings
#' @export

layout_word <- function(word, data, shift = 1) {

  tibble::tibble(character = stringr::str_split(word, "")[[1]]) %>%
    dplyr::left_join(data, by = "character") %>% tidyr::drop_na() %>%
    dplyr::mutate(d = seq(0, by = shift, length = dplyr::n())) %>%
    dplyr::mutate(layout = purrr::map2(layout, d, ~ dplyr::mutate(..1, x = x + ..2))) %>%
    tidyr::unnest(layout)
}


# define a character map and randomly modify few characters in y scaling
#' @export

gen_charmap <- function(
  n = 26, n_control = 4, n_tall = 4, size_tall = 4,
  scale = 0.5, rotation = -pi/6) {

  data_map <- tibble::tibble(
    pattern = 1:(n + 3),
    character = c(letters[1:n], ".", ",", "?"),
    r = dplyr::case_when(
      rbinom((n + 3), 1, p = n_tall/(n + 3)) == 0 ~ 1,
      TRUE ~ size_tall)
    ) %>%
    dplyr::mutate(layout = purrr::map(
      r, ~ layout_ellipse(n = n_control, r = ., scale_x = scale, a = rotation)))

  return(data_map)

}

# draw a spline curve directly on layout points
#' @export

render_spline <- function(
  data, type = "clamped", n = 100, orientation = "h",
  width = 0.5, alpha = 1, coord = NULL) {

  switch(
    orientation,

    h = {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(x, y)) +
        ggforce::geom_bspline(
          lineend = "round", type = type, n = n, size = width, alpha = alpha)
    },

    v = {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(x = y, y = -x)) +
        ggforce::geom_bspline(
          lineend = "round", type = type, n = n, size = width, alpha = alpha)
    }
  )

  return(plot + coord + ggplot2::theme_void())

}


# render a paragraph from an existing text and a set of plots using script style
#' @export

render_script <- function(
  text, data, length = 80, scale = 0.9,
  orientation = "h", align = "none") {

  # split text to characters
  seq <- text %>% stringr::str_to_lower() %>% stringr::str_split(., "")

  # map characters to glyph plots, non-match creates NULL plot
  # randomly select one variation per character.
  glyph <- tibble::tibble(character = seq[[1]]) %>%
    dplyr::mutate(position = seq_along(character)) %>%
    dplyr::left_join(data %>% dplyr::select(character, pattern, variation, plot)) %>%
    dplyr::group_by(position, character) %>%
    dplyr::slice_sample(n = 1)

  # render the character string
  switch (
    orientation,

    h = {
      cowplot::plot_grid(
        plotlist = glyph$plot, ncol = length,
        scale = scale, align = align)
      },

    v = {
      cowplot::plot_grid(
        plotlist = glyph$plot, nrow = length,
        scale = scale, align = align, byrow = FALSE)
      }
  )

}


# render a line of glyphs - words
#' @export

render_line <- function(data, ncol = 80, scale = 0.9) {
  cowplot::plot_grid(
    plotlist = c(data$plot, list(NULL)),
    rel_widths = c(data$length, ncol - sum(data$length)),
    ncol = nrow(data) + 1, scale = scale) +
    ggplot2::theme(plot.margin = grid::unit(c(2,0,2,0), "mm"))
}

# render a paragraph using cursive style
#' @export

render_cursive <- function(
  text, data, ncol = 80, shift = 1,
  size = 0.5, n_points = 200, scale = 0.8) {
  # split text to words
  seq <- text %>% stringr::str_to_lower() %>% stringr::str_split(., " ")

  # generate words from glyph concatenation
  # define line as groups of words of given cumulative length
  words <- tibble::tibble(word = seq[[1]]) %>%
    dplyr::mutate(
      position = seq_along(word),
      length = stringr::str_length(word),
      cl = purrr::accumulate(length, ~ dplyr::if_else(.x > ncol, .y, .x + .y)),
      line = ifelse(dplyr::lag(cl, default = 0) > ncol, 1, 0) %>% cumsum()
      ) %>%
    dplyr::mutate(
      glyph = purrr::map(word, ~ layout_word(., data = data, shift = shift)),
      plot = purrr::map(
        glyph, ~ render_spline(., width = size, n = n_points, type = "open")
        )
    )

  lines <- words %>%
    dplyr::group_by(line) %>% tidyr::nest() %>%
    dplyr::mutate(plot = purrr::map(data, ~ render_line(., ncol, scale = scale)))

  cowplot::plot_grid(plotlist = lines$plot, nrow = nrow(lines), scale = scale)
}
