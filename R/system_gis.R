# input ####

#' Read and merge DEM tiles from IGN DBALTI or RGEALTI as a function of WGS84 coordinates
#' @param coord latitude and longitude of centroid (named numeric vector, decimal degrees).
#' @param buffer circular buffer around the centroid (numeric, m).
#' @param dep departement code number used to index DEM data (numeric).
#' @param source DEM database to be used (character).
#' * "db_alti" is a national database at 25x25m resolution.
#' * "rge_alti" is a departement-level database at 5x5m resolution, with 1 - 20m source data.
#' @return a digital elevation model as a spatial data object (stars)
#' @export
#'
read_dem <- function(coord, buffer = 1000, dep, source="db_alti"){

  # define map location in IGN CRS
  location <- if (is.vector(coord)) {
    data.frame(
      lon = coord["lon"],
      lat = coord["lat"]) %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326) %>%
      sf::st_transform(crs = 2154)
  } else coord

  # load grid corresponding to selected data source
  switch(
    source,

    db_alti = {
      grid <- sf::read_sf(glue::glue("data/private/{source}/shp/dalles.shp"))
    },

    rge_alti = {
      # TODO get departement code corresponding to coordinates
      # https://geo.api.gouv.fr/adresse
      # TODO handle cases with cross departements datasets
      grid <- sf::read_sf(glue::glue("data/private/{source}/shp/{dep}/dalles.shp"))
    }
  )

  sf::st_agr(grid) = "constant"

  # list tile containing requested location plus buffer
  list_tiles <- sf::st_intersection(
    grid,
    location %>% sf::st_buffer(buffer)
    ) %>%
    dplyr::pull(NOM_DALLE)

  # read corresponding tiles and merge them
  dem_list <- tibble::tibble(
      path = glue::glue("data/private/{source}/data/{list_tiles}.asc")
    ) %>%
    dplyr::mutate(
      dem = purrr::map(
        path, ~ stars::read_stars(.) %>% purrr::set_names("z") %>% sf::st_set_crs(2154)
        )
      )

  dem <- purrr::reduce(dem_list$dem, stars::st_mosaic)

  return(dem)

}

# processing ####

#' Filter ridgeline dataframe as a function of segment length
#' @param data dataframe of computed ridgelines from `render_ridge()`
#' @param length_n length of ridge segments to be filtered (integer, cells)
#' @param length_x relative length used as threshold for removal of ridgeline (numeric, 0-1)
#' @param dist_y relative distance to remove ridgelines (numeric, 0-1)
#' @export
#'
filter_ridge_length <- function(
  data,
  length_n = 10,
  length_x = 5/100,
  dist_y = 0.95
) {

  # filter for ridge elements shorter than a threshold (cell number)
  data_cell <- data %>% dplyr::ungroup() %>% dplyr::arrange(y,x) %>%
    dplyr::mutate(zl = cumsum(is.na(zn))) %>%
    dplyr::group_by(zl) %>% dplyr::mutate(zl_n = dplyr::n()) %>% dplyr::ungroup() %>%
    dplyr::mutate(zn = dplyr::if_else(zl_n <= length_n, NA_real_, zn))

  # filter for ridge lines with a low contribution
  data_line <- data %>%
    dplyr::group_by(y_rank, y_dist) %>%
    dplyr::summarise(xp = sum(!is.na(zn)) / dplyr::n(), .groups = "drop") %>%
    dplyr::filter(xp < length_x, y_dist > dist_y)

  data_filter <- dplyr::anti_join(data_cell, data_line, by = c("y_rank", "y_dist"))

  return(data_filter)

}

#' Filter ridgeline dataframe as a function of local slope
#' @param data dataframe of computed ridgelines from `render_ridge()`
#' @param size size of window used to compute the slope rolling average (integer, cells)
#' @export

filter_ridge_slope <- function(
  data,
  size = 3
) {

  data_filter <- data %>%
    dplyr::group_by(y_rank) %>%
    dplyr::mutate(
      z_slope = abs(
        (zn - dplyr::lag(zn, default = 0)) / (xn - dplyr::lag(xn, default = 0))
        ),
      z_slope = RcppRoll::roll_mean(z_slope, n = size, fill = NA_real_),
      zn = dplyr::if_else(z_slope == 0, NA_real_, zn)
    ) %>% dplyr::ungroup()


  return(data_filter)

}


#' Filter ridgeline dataframe as a function of rank index
#' @param data dataframe of computed ridgelines from `render_ridge()`
#' @param p proportion of ridgeline to remove
#' @param method method used to remove ridges.
#' * "random" randomly removes a proportion of ridges.
#' * "grid" uniformly sample a proportion of ridges.
#' * "strip" remove a proportion of consecutive ridges.
#' @export

filter_ridge_rank <- function(data, p, method = "grid") {

  switch(
    method,

    random = {
      dplyr::inner_join(
        data,
        data %>% dplyr::distinct(y_rank) %>%
          dplyr::slice_sample(prop = p)
      )
    },

    grid = {
      dplyr::inner_join(
        data,
        data %>% dplyr::distinct(y_rank) %>%
          dplyr::slice(
            seq(1, dplyr::n(), len = p * dplyr::n()) %>%
              as.integer()
          )
      )
    },

    strip = {
      dplyr::inner_join(
        data,
        data %>% dplyr::distinct(y_rank) %>%
          dplyr::slice(
            sample(1:n(), p * n()) %>%
              purrr::map(~ .x:(.x + 40)) %>% purrr::flatten_int()
          )
      )
    }
  )
}


# rendering ####

#' Iterate to create geom_sf layers as a function of a dataframe
#' @param data sf multipolygon object for shore, used to compute waterlines.
#' @param n number of waterlines.
#' @param d0,r  buffer and rate of buffer growth between water lines.
#' @param color,scale color and scaling of plotted lines.
#' @param ... used for pmap compatibility
#' @export

geom_waterline <- function(
  data, n = 5, r = 2, d0 = 50,
  color="black", scale = 1, ...) {

  plot <- tibble::tibble(
    buffer = purrr::accumulate(seq(d0, by=1, length.out = n), ~ . * r),
    shade = seq(0.5, 0.9, length.out = n)
  ) %>%
    dplyr::mutate(
      layers = purrr::map2(
        buffer, shade,
        ~ ggplot2::geom_sf(
          data = data %>% sf::st_buffer(.x), # %>% st_crop(data),
          fill = NA, color = colorspace::lighten(color, .y), size = 1/10 * scale)
      ))

  return(plot$layers)

}


#' Render a DEM as contour plot with waterlines
#' @param data a digital elevation model as a spatial data object (stars) from `read_dem()`
#' @param coord geographical coordinates of the centroid (named vector with lon, lat)
#' @param area_min minimal area for polygons to be considered (m^2)
#' @param length_min minimal length for contour lines to be considered (m)
#' @param alt_min,alt_max,alt_contour minimal, maximal, and elevation step used to define contour lines (m)
#' @param n_water number of waterlines
#' @param scale scaling coefficient of contour and water lines
#' @param outline if TRUE render and plot only shorelines (boolean)
#' @param layers if TRUE return all layers in different objects (boolean)
#' @param ... used for pmap compatibility
#' @return a ggplot object
#' @export

render_contour <- function(
  data, coord, area_min = 2e4, length_min = 600,
  alt_min, alt_max, alt_contour = 100,
  n_water = 5, scale = 1,
  outline = FALSE, layers = FALSE, ...) {

  # define a sf point object corresponding to given coordinates
  point <- data.frame(lon = coord["lon"], lat = coord["lat"]) %>%
    sf::st_as_sf(coords = c("lon", "lat")) %>%
    sf::st_set_crs(4326) %>% sf::st_transform(crs = 2154)

  # define map layers with contour lines
  # TODO simplify polygons to speed up plotting with rmapshaper::ms_simplify
  # TODO crop before contour
  # TODO test isoband package to speed up calculations

  layer_shore <- data %>%
    stars::st_contour(breaks = c(0, alt_min), contour_lines = TRUE) %>%
    sf::st_crop(buffer_rectangle(point, ...)) %>%
    dplyr::filter(sf::st_length(.) > units::set_units(length_min,"m"))

  bb <- sf::st_bbox(layer_shore)

  switch(
    as.character(outline),

    "FALSE" = {

      breaks_major <- seq(alt_min, alt_max, by = alt_contour)

      breaks_minor <- seq(alt_min, alt_max, by = alt_contour / 2)

      # get a multipolygon object for water, used to compute waterlines
      layer_water <- data %>%
        stars::st_contour(breaks = c(0, alt_min)) %>%
        sf::st_buffer(dist = 0) %>%
        sf::st_crop(buffer_rectangle(point, ...)) %>%
        sf::st_cast("MULTIPOLYGON") %>% dplyr::slice(2) %>%
        sf::st_cast("POLYGON") %>%
        dplyr::filter(sf::st_area(.) > units::set_units(area_min,"m^2")) %>%
        sf::st_combine()

      layer_contour_minor <- data %>%
        stars::st_contour(
          breaks = breaks_minor[! breaks_minor %in% breaks_major],
          contour_lines = TRUE) %>%
        sf::st_crop(buffer_rectangle(point, ...)) %>%
        dplyr::filter(sf::st_length(.) > units::set_units(length_min,"m"))

      layer_contour_major <- data %>%
        stars::st_contour(
          breaks = breaks_major[! breaks_major %in% alt_min],
          contour_lines = TRUE) %>%
        sf::st_crop(buffer_rectangle(point, ...)) %>%
        dplyr::filter(sf::st_length(.) > units::set_units(length_min,"m"))
      },

    "TRUE" = {}
  )

  # plot contour lines

  switch(
    as.character(outline),

    "FALSE" = {

      plot_waterline <- geom_waterline(data = layer_water, n = n_water, ...)

      plot_contour_minor <- ggplot2::geom_sf(
        data = layer_contour_minor,
        color = colorspace::lighten("black", 0.6), size=1/10 * scale)

      plot_contour_major <- ggplot2::geom_sf(
        data = layer_contour_major,
        color = colorspace::lighten("black", 0.4), size=2/10 * scale)

      plot_shore <- ggplot2::geom_sf(
        data = layer_shore,
        color="black", size = 4/10 * scale)

      coord_box <- ggplot2::coord_sf(xlim=c(bb$xmin, bb$xmax), ylim=c(bb$ymin, bb$ymax))

      plot <- ggplot2::ggplot() + plot_waterline + plot_contour_minor +
        plot_contour_major + plot_shore + ggplot2::theme_void()

      plot_layers <- list(
        shore = plot_shore,
        contour_minor = plot_contour_minor,
        contour_major = plot_contour_major,
        waterline = plot_waterline,
        coord_box = coord_box)

    },
    "TRUE" = {
      plot <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = layer_shore,
          color="black", size = 4/10 * scale) +
        ggplot2::theme_void()
    }
  )

  switch(
    as.character(layers),
    "TRUE" = {return(plot_layers)},
    "FALSE" = {return(plot)}
  )

}

#' Render a DEM with elevation as a function of longitude, grouped per latitude (ridge plot)
#' @param data dataframe from a digital elevation model (x,y,z)
#' @param n_ridges number of ridges uniformly selected, value 0 select all ridges in dataset  (integer)
#' @param n_drop number of ridges to drop in distance (integer)
#' @param n_lag depth of search for line removal algorithm (number of successive ridges)
#' @param z_shift distance on y-axis used to shift successive ridges (m)
#' @param z_threshold minimal distance threshold to remove points
#'  between successive ridges (m)
#' @param z_k scaling coefficient for perspective computation. a value of 0 switch to linear perspective.
#' @return a dataframe suitable for plotting in two dimensions :
#' * x, longitude (m)
#' * y, latitude (m)
#' * z, altitude (m)
#' * xn, relative longitude (m)
#' * y_rank, relative latitude
#' * y_dist, relative latitude (0,1)
#' * dz, shift in altitude (m)
#' * zs, shifted altitude (m)
#' * z_rank, relative altitude (ranked per longitude)
#' * zn, shifted altitude, after occlusion (m)
#' * zl_n, length of the current ridge (cell)

#' @export

render_ridge <- function(
  data,
  n_ridges = 200,
  n_drop = 0,
  n_lag = 100,
  z_shift = 15,
  z_threshold = 10,
  z_k = 0
  ){

  # set n_ridges to max number in data if parameter is 0
  n_ridges <- ifelse(n_ridges == 0, data %>% dplyr::distinct(y) %>% nrow(), n_ridges)

  # keep a fixed number of distinct ridges
  # compute elevation shift as a function of normalized distance
  data_index <- data %>%
    dplyr::distinct(y) %>% dplyr::arrange(y) %>%
    dplyr::slice(seq(1, (dplyr::n() - n_drop), len = n_ridges) %>% as.integer()) %>%
    dplyr::mutate(
      y_rank = rank(y),
      y_dist = scales::rescale(y, to = c(0,1)),
    ) %>%
    dplyr::mutate(dz = f_sig(y_dist, k = z_k, a = 2, b = 0) * y_rank * z_shift)

  # compute z shift as a function of ridge index
  data_shift <- data %>%
    dplyr::inner_join(data_index, by = "y") %>%
    dplyr::group_by(y) %>%
    dplyr::mutate(xn = x - min(x), x_rank = rank(x)) %>% dplyr::ungroup() %>%
    dplyr::arrange(y) %>% dplyr::group_by(x) %>%
    dplyr::mutate(
      zs = z + dz,
      z_rank = rank(zs)
    )

  # define window functions for multiple lag positions
  list_distance <- purrr::map(glue::glue("~ . - lag(., n = {1:n_lag})"), ~ as.formula(.))
  list_col <- glue::glue("zs_{1:n_lag}")

  # remove points hidden by foreground ridges :
  # distance between successive y for each x is less than a threshold (default 0)
  data_ridge <- data_shift %>%
    dplyr::mutate(dplyr::across(zs, .fns = list_distance)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      zn = dplyr::case_when(
        dplyr::if_any(tidyselect::all_of(list_col), ~ . < z_threshold) ~ NA_real_,
        TRUE ~ zs)) %>%
    dplyr::select(- tidyselect::all_of(list_col))

  return(data_ridge)

}

# filter ridgeline dataframe on line length
#' @export


