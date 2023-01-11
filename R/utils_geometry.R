
# geometry ####

#' Get bounding box from 2D dataframes
#' @param data dataframe with x and y coordinates
#' @param vars alternative names for x and y variables (character vector)
#' @param method type of output :
#'   * center : returns the coordinates of center, width and height of the bounding box
#'   * points : returns the coordinates of cardinal points
#' @return bounding box attributes
#' @export
get_box <- function(data, vars = c("x","y"), method = "center") {

  data <- data |> dplyr::select(x = {{vars}}[1], y = {{vars}}[2])

  switch(
    method,

    center = {

      geo <- list(
        x0 = mean(range(data$x, na.rm = TRUE)),
        y0 = mean(range(data$y, na.rm = TRUE)),
        x = diff(range(data$x, na.rm = TRUE)),
        y = diff(range(data$y, na.rm = TRUE))
      )

    },

    points = {

      xr = range(data$x, na.rm = TRUE)
      yr = range(data$y, na.rm = TRUE)

      geo <- dplyr::tibble(
        x = c(xr[1], xr[1], xr[2], xr[2]),
        y = c(yr[1], yr[2], yr[2], yr[1])
      )

    },
    stop("Invalid `method` value")
  )

  return(geo)

}


#' Translate a 2D object
#' @param data dataframe with x and y coordinates
#' @param x0,y0 shift in x and y axis (units)
#' @return a dataframe with new coordinates columns
#' @export

translate <- function(data, x0, y0) {
  tr <- ggforce::linear_trans(translate(x0, y0))
  tibble::tibble(id = data$id, tr$transform(data$x, data$y, x0, y0))
}

#' Rotate a 2D object
#' @param data dataframe with x and y coordinates
#' @param a rotation angle (radian)
#' @return a dataframe with new coordinates columns
#' @export

rotate <- function(data, a) {
  tr <- ggforce::linear_trans(rotate(a))
  tibble::tibble(id = data$id, tr$transform(data$x, data$y, a))
}

#' Rotate then translate a 2D object
#' @param data dataframe with x and y coordinates
#' @param x0,y0 shift in x and y axis (units)
#' @param a rotation angle (radian)
#' @return a dataframe with new coordinates columns
#' @export

r_t <- function(data, x0, y0, a) {

  if (nrow(data) > 0) {
    data |> rotate(a) |> translate(x0, y0)
  } else {tibble::tibble()}

}


# spatial ####

#' Convert a dataframe to a sf point object
#' @param data dataframe with x and y coordinates
#' @return a sf point object, NULL if input dataframe is empty
#' @export

as_sf <- function(data) {

  if (nrow(data) == 0) {return(NULL)}

  else {
    data |>
      dplyr::select(tidyselect::any_of(c("id", "x", "y"))) |>
      sf::st_as_sf(coords = c("x","y"))
  }
}

#' Convert a sf point object to sf polygon
#' @param data sf point object
#' @return a sf polygon object
#' @export

as_sf_polygon <- function(data) {
  as_sf(data) |>
    dplyr::summarise(geometry = sf::st_combine(geometry)) |>
    sf::st_cast("POLYGON")
}


#' Define a rectangle bounding box with a given ratio around a location
#' @param data sf point object
#' @param size size of the largest dimension of the bounding box (units)
#' @param x_shift,y_shift shifts in the x and y direction (units)
#' @param orientation orientation of the bounding box ("h" or "v")
#' @param ratio height:width ratio
#' @param ... used for mapping
#' @return a sf bounding box object
#' @export

buffer_rectangle <- function(
  data, size = 60000, x_shift=0, y_shift=0,
  orientation="h", ratio = 297/210, ...){

  switch(
    orientation,


    h = {
      x_size = size / 2
      y_size = (size / ratio) / 2},

    v = {
      x_size = (size / ratio) / 2
      y_size = size / 2},

    stop("Invalid `orientation` value")
  )

  sf::st_bbox(data) +
    c(- x_size, - y_size, x_size, y_size) +
    c(x_shift, y_shift, x_shift, y_shift)
}

# geoms ####

#' Render a rectangular frame around a 2D object.
#' @param data data frame with x and y columns, it could be the plotted data or just the limits for the frame.
#' @param jitter amount of jitter to add to sampled points in the frame
#' @param scaling scaling factor around the object bounding box
#' @param size,color aesthetics passed to geom_path()
#' @return a geom_path ggplot layer.
#' @export
render_frame <- function(
    data, jitter = 10, scaling = 0.1,
    size = 0.5, color = "black") {

  # compute frame data
  data <- purrr::pmap_df(
    get_box(data),
    ~ sample_rectangle(
      x0 = ..1, y0 = ..2, x = ..3, y = ..4,
      jitter = jitter, scaling = scaling)
    )

  layer <- ggplot2::geom_path(
    ggplot2::aes(x,y), data = data, linewidth = size, color = color)

  return(layer)

}

# themes ####

#' Customize a theme object with margins, color, and background.
#' @param output set up background for "paper" or "plotter" output.
#' @param color background color for paper themes.
#' @param margin a vector of four numeric values for margins (top, right, bottom, left) in mm.
#' @return a ggplot theme object
#' @export
theme_paper <- function(output = "paper", color = "#FEFAEE", margin = rep(0,4)) {

  switch (
    output,
    paper = {
      theme <- ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = color, linewidth = NA, colour = NA),
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
  )

  return(theme)

}
