
# geometry ####

#' Set common limits to a list of plots
#' @param p a list of plots
#' @return a list of plots with modified limits.
#' @export
set_range <- function(p) {

  yr = purrr::map(p, ~layer_scales(..1)$y$get_limits()) |>
    unlist() |> range()

  xr = purrr::map(p, ~layer_scales(..1)$x$get_limits()) |>
    unlist() |> range()

  p |> purrr::map(~..1 + xlim(xr) + ylim(yr))
}

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
#' @param data dataframe with x and y coordinates, and id columns
#' @param x0,y0 shift in x and y axis (units)
#' @param index string for the index column to be added from the original data
#' @return a dataframe with new coordinates columns
#' @export
#'
tr_translate <- function(data, x0, y0, index = NULL) {

  tr <- ggforce::linear_trans(translate(x0, y0))
  d_tr <- tibble::tibble(tr$transform(data$x, data$y, x0, y0))

  if (is.null(index)) {d_tr} else {
    tibble::tibble("{index}" := dplyr::pull(data, index), d_tr)
  }
}

#' Rotate a 2D object
#' @param data dataframe with x and y coordinates, and id columns
#' @param a rotation angle (radian)
#' @param index string for the index column to be added from the original data
#' @return a dataframe with new coordinates columns
#' @export
#'
tr_rotate <- function(data, a, index = NULL) {
  tr <- ggforce::linear_trans(rotate(a))

  d_tr <- tibble::tibble(tr$transform(data$x, data$y, a))

  if (is.null(index)) {d_tr} else {
    tibble::tibble("{index}" := dplyr::pull(data, index), d_tr)
  }
}

#' Rotate then translate a 2D object
#' @param data dataframe with x and y coordinates, and id columns
#' @param x0,y0 shift in x and y axis (units)
#' @param a rotation angle (radian)
#' @param index string for the index column to be added from the original data
#' @return a dataframe with new coordinates columns
#' @export

tr_rt <- function(data, x0, y0, a, index = NULL) {

  if (nrow(data) > 0) {
    data |> tr_rotate(a, index) |> tr_translate(x0, y0, index)
  } else {tibble::tibble()}

}

#' Add jitter to a 2D object
#' @param data dataframe with x and y coordinates, and id columns
#' @param a jitter amount (units)
#' @param index string for the index column to be added from the original data
#' @return a dataframe with new coordinates columns
#' @export
#'
tr_jitter <- function(data, a, index = NULL) {

  d_tr <- dplyr::mutate(data, dplyr::across(x:y, ~ jitter(.x, amount = a)))

  if (is.null(index)) {d_tr} else {
    tibble::tibble("{index}" := dplyr::pull(data, index), d_tr)
  }

}

#' Apply a periodic transformation to a 2D object
#' @param data dataframe with x and y coordinates
#' @param period period for cyclic offset
#' @param amplitude strength of offset
#' @param delta control how x and y offsets are linked
#' @return a dataframe with new coordinates columns
#' @export
#'
tr_wave <- function(data, period = 30, amplitude = 1/10, delta = 0){

  if (nrow(data) > 2) {
    width <- diff(range(data$x))
    height <- diff(range(data$y))

    # create basic sine wave variation
    knots <- seq(min(data$x), max(data$x), length.out = max(5, ceiling(width)))
    variation <- sin((2 * pi / period) * knots) * amplitude

    # create smooth interpolation function
    f_spline <- stats::smooth.spline(knots, variation)

    # apply to both x and y, with phase shift for y
    data_tr <- data |>
      dplyr::mutate(
        x = x + stats::predict(f_spline, x)$y * width ,
        y = y + stats::predict(f_spline, x + delta)$y * height
      )

    return(data_tr)

  } else {

    return(data)

  }

}

#' smooth paths with loess method 
#' @param data a dataframe with x and y columns
#' @param span controls the degree of smoothing. higher value creates smoother paths.
#' @param n_min minimal number of points for smoothing.
#' @param ... use for mapping other arguments
#' @export
#' 
tr_loess <- function(data, span, n_min = 10, ...) {

  # do not fit model for small paths
  if(nrow(tidyr::drop_na(data, y)) > n_min) {

    m <- stats::loess(y ~ x, data = data, na.action = stats::na.exclude, span = span)
    return(data |> dplyr::mutate(y = stats::predict(m)) |> dplyr::select(x, y))

  } else {
    return(data |> dplyr::mutate(y = rep(NA, dplyr::n())))
  }
  
}

#' apply recursive transformations
#' @param data dataframe with xy columns
#' @param f transformation function
#' @param p named list of parameters values for the transformation function
#' @param ... use for mapping other arguments
#' @export
#'  
tr_recurse <- function(data, f, p, ...) {
  p |> purrr::pmap_dfr(~ f(data, ...), .id = "id")
}


# spatial ####

#' Convert a dataframe to a sf point object
#' @param data dataframe with x and y coordinates
#' @return a sf point object, NULL if input dataframe is empty
#' @export
#' 
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
#' @param size size of the largest dimension of the bounding box. a vector of length 2 (width, height) ignores the orientation and ratio parameters
#' @param x_shift,y_shift shifts in the x and y direction
#' @param orientation orientation of the bounding box ("h" or "v")
#' @param ratio height:width ratio
#' @param ... used for mapping
#' @return a sf bounding box object
#' @export

buffer_rectangle <- function(
  data, size = 60000, x_shift = 0, y_shift = 0,
  orientation = "h", ratio = 297/210, ...){

  #
  if (length(size) == 1) {
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
  } else {
    x_size = size[1] / 2
    y_size = size[2] / 2
  }


  sf::st_bbox(data) +
    c(- x_size, - y_size, x_size, y_size) +
    c(x_shift, y_shift, x_shift, y_shift)
}

#' Crop a raster as a function of given bounding box
#' @param raster, input raster (stars or dataframe). With a dataframe, the cropping is done around the center point.
#' @param point, centroid of the cropping region. point and raster CRS must be identical.
#' @param size, dimensions defining the cropping bounding box (x,y raster units)
#' @return xyz dataframe of the cropped region
#' @export
crop_rectangle <- function(raster, point, size) {

  if (is.data.frame(raster)) {

    x0 = sum(range(raster$x))/2
    y0 = sum(range(raster$y))/2

    crop <- raster |>
      dplyr::filter(dplyr::between(x, x0 - size[1] / 2, x0 + size[1] / 2 )) |>
      dplyr::filter(dplyr::between(y, y0 - size[2] / 2, y0 + size[2] / 2 ))

  } else {

    box <- buffer_rectangle(point, size = size)
    crop <- sf::st_crop(raster, box) |>
      dplyr::as_tibble() |> dplyr::select(x, y, z = 3)
  }

  return(crop)
}


# geoms ####

#' Render an irregular rectangular frame around a 2D object.
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

