
# geometry ####

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

r_t <- function(data, x0, y0, a) { data %>% rotate(., a) %>% translate(., x0, y0)}


# spatial ####

#' Convert a dataframe to a sf point object
#' @param data dataframe with x and y coordinates
#' @return a sf point object, NULL if input dataframe is empty
#' @export

as_sf <- function(data) {

  if (nrow(data) == 0) {return(NULL)}

  else {
    data %>%
      dplyr::select(tidyselect::any_of(c("id", "x", "y"))) %>%
      sf::st_as_sf(coords = c("x","y"))
  }
}

#' Convert a sf point object to sf polygon
#' @param data sf point object
#' @return a sf polygon object
#' @export

as_sf_polygon <- function(data) {
  as_sf(data) %>%
    dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
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
      y_size = size / 2}
  )

  sf::st_bbox(data) +
    c(- x_size, - y_size, x_size, y_size) +
    c(x_shift, y_shift, x_shift, y_shift)
}

