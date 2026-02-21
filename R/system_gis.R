
# projection system ####

## helpers ####

#' Sliding maximum of the n previous values
#'
#' Pads x with n values of -Inf so roll_max always has a full window at group
#' boundaries (-Inf is the identity element for max). Returns a vector of same length as x.
#' @param x numeric vector
#' @param n window size (number of previous values to look back)
roll_max_lag <- function(x, n) {
  c(rep(-Inf, n), x) |>
    RcppRoll::roll_max(n = n, fill = NA_real_, align = "right") |>
    utils::tail(-n)
}

## io ####

#' Read DEM tiles from IGN public elevation datasets
#' @param coord sf point in EPSG:2154, or named vector with lon/lat in decimal degrees
#' @param buffer circular buffer around the centroid (numeric, m).
#' @param source DEM database to be used (character).
#' * "db_alti" is a national database at 25x25m resolution.
#' * "rge_alti" is a gridded database at 5x5m resolution, with 1 - 20m source data.
#' @return stars object
#' 
read_tiles <- function(coord, buffer, source) {

  # load grid corresponding to selected data source
  grid <- switch(
    source,
    db_alti = {sf::read_sf(glue::glue("data/private/{source}/shp/dalles.shp"))},
    rge_alti = {sf::read_sf(glue::glue("data/private/{source}/shp/rge_alti_5m.shp"))},
    stop("Invalid `source` value")
  )

  # do thing with CRS
  sf::st_agr(grid) = "constant"

  # list tile containing requested location plus buffer
  list_tiles <- sf::st_intersection(
    grid |> sf::st_transform(crs = 2154),
    coord |> sf::st_buffer(buffer)
    ) |>
    dplyr::pull(NOM_DALLE)

  # read corresponding tiles and merge them
  dem_list <- tibble::tibble(
      path = glue::glue("data/private/{source}/data/{list_tiles}.asc")
    ) |>
    dplyr::mutate(
      dem = purrr::map(
        path, ~ stars::read_stars(.) |> purrr::set_names("z") |> sf::st_set_crs(2154)
        )
      )

  dem <- purrr::reduce(dem_list$dem, stars::st_mosaic)
}

#' Read and crop DEM data from multiple sources
#'
#' Unifies all elevation data sources and crops to an oriented bounding box computed from the center coordinate.
#' @param coord sf point in EPSG:2154, or named vector with lon/lat in decimal degrees.
#' @param size size of the largest dimension of the bounding box (numeric, m).
#'   A vector of length 2 (width, height) ignores the orientation and ratio parameters.
#' @param zoom zoom level (1-14) for *elevatr* API. For details on zoom and resolution see the documentation from Mapzen.
#' @param x_shift horizontal offset applied to the cropping box center (m, default 0)
#' @param y_shift vertical offset applied to the cropping box center (m, default 0)
#' @param orientation cropping box orientation, "v" (vertical) or "h" (horizontal). Default "v".
#' @param ratio width:height ratio for the cropping box (default 297/210, A4 portrait)
#' @param source source for elevation data (memory, local or online sources)
#'   * "raster": pre-loaded raster object passed via `data`
#'   * "db_alti": French IGN BD ALTI 75x75m
#'   * "rge_alti": French IGN RGE ALTI 5x5m, with 1-20m source input.
#'   * "api": elevatr package, EU-DEM / Copernicus (~30m, zoom-dependent)
#' @param data raster object, used when source = "raster" to sample and crop an object in memory (default NULL)
#' @return a digital elevation model as a cropped spatial data object (stars)
#' @export
#'
read_dem <- function(
  coord, size, zoom, 
  x_shift = 0, y_shift = 0, orientation = "v", ratio = 297/210, 
  source = "db_alti", data = NULL){

  # define center for reading tiles in IGN CRS
  coord <- if (is.vector(coord)) {
    data.frame(
      lon = coord["lon"],
      lat = coord["lat"]) |>
      sf::st_as_sf(coords = c("lon", "lat")) |>
      sf::st_set_crs(4326) |>
      sf::st_transform(crs = 2154)
  } else coord
  
  # read elevation data from memory, local or online sources
  dem <- switch (
    source,
    raster = {data},
    db_alti = {read_tiles(coord = coord, buffer = size, source = source)},
    rge_alti = {read_tiles(coord = coord, buffer = size * 0.6, source = source)},
    api = {suppressWarnings(elevatr::get_elev_raster(coord, z = zoom, expand = max(size) * 0.5))},
    stop("Invalid `source` value")
  )
  
  # crop elevation raster
  box <- buffer_rectangle(
    coord, size = size, x_shift = x_shift, y_shift = y_shift,
    orientation = orientation, ratio = ratio)
  
  dem <- dem |> stars::st_as_stars(proxy = FALSE) |> sf::st_crop(box)

  return(dem)

}


#' Rotate and scale a stars DEM
#'
#' Applies the cardinal-direction rotation so y increases toward the viewer,
#' then scales elevation values.
#' @param dem stars object
#' @param view viewer's cardinal position relative to terrain ("N", "S", "E", "W").
#'   E.g. view = "S" means viewer is south, looking northward.
#' @param z_scale scaling factor applied to elevation values (default 1)
#' @param output output format: terra SpatRaster ("raster") or xyz tibble ("df")
#' @return terra SpatRaster (rotated and scaled) or xyz tibble depending on `output`
#' @export
transform_dem <- function(dem, view = "N", z_scale = 1, output = "raster") {
  
  d <- dem |> terra::rast()
  
  d_rot <- switch(EXPR = view,
    N = d |> terra::flip("vertical") |> terra::flip("horizontal"),
    S = d,
    E = d |> terra::trans() |> terra::flip("horizontal"),
    W = d |> terra::trans() |> terra::flip("vertical"),
    stop("Invalid `view` value")
  )

  d_scale <- switch(
    output,
    "raster" = {d_rot * z_scale},
    "df" = {(d_rot * z_scale) |> as.data.frame(xy = TRUE) |> dplyr::rename(z = 3) |> tibble::as_tibble()}
  )

  return(d_scale)

}


## projection ####

#' Select ridge lines and compute vertical shift
#'
#' Steps 1-2 of the ridge pipeline: samples n_lines evenly from the y-axis,
#' assigns ranks and normalized distances, computes a perspective shift dz,
#' and adds the shifted column \{z\}s = \{z\} + dz.
#'
#' @param data dataframe with x, y, \{z\} columns
#' @param z base column name for elevation (default "z"). Output column: \{z\}s
#' @param n_lines number of lines to select (0 = all)
#' @param n_drop number of lines to drop from the far end
#' @param shift vertical shift per rank (m)
#' @param k non-linear perspective coefficient (0 = linear)
#' @return input dataframe with additional columns:
#'   * `y_rank`: integer rank of each line from foreground (1) to background
#'   * `y_dist`: normalized line position in 0:1
#'   * `dz`: vertical shift applied to this line (m)
#'   * `xn`: x coordinate relative to the left edge of each line (m)
#'   * `x_rank`: integer rank of each point within its line
#'   * `\{z\}s`: shifted elevation (\{z\} + dz)
#' @export
shift_lines <- function(data, z = "z", n_lines = 200, n_drop = 0, shift = 15, k = 0) {

  z_out <- paste0(z, "s")
  n_lines <- ifelse(n_lines == 0, nrow(dplyr::distinct(data, y)), n_lines)

  data_index <- data |>
    dplyr::distinct(y) |>
    dplyr::arrange(y) |>
    dplyr::slice(as.integer(seq(1, dplyr::n() - n_drop, len = n_lines))) |>
    dplyr::mutate(
      y_rank = rank(y),
      y_dist = scales::rescale(y, to = c(0, 1))
    ) |>
    dplyr::mutate(dz = f_sig(y_dist, k = k, a = 2, b = 0) * y_rank * shift)

  data |>
    dplyr::inner_join(data_index, by = "y") |>
    dplyr::group_by(y) |>
    dplyr::mutate(xn = x - min(x), x_rank = rank(x)) |>
    dplyr::ungroup() |>
    dplyr::arrange(y) |>
    dplyr::mutate(!!z_out := .data[[z]] + dz)
}

#' Stack shifted layers vertically
#'
#' Normalizes each layer so \{z\}s starts at 0, then offsets layers sequentially.
#' Layer 1 = foreground, layer N = background. After stacking, apply mask_lines()
#' across the combined dataframe for cross-layer occlusion.
#'
#' @param layers list of dataframes from shift_lines()
#' @param z base column name (default "z"). Operates on \{z\}s and y_rank
#' @param z_stack stacking coefficient: 1 = adjacent layers, <1 = overlap, 0 = superposed
#' @param offsets optional numeric vector of manual offsets between layers (overrides z_stack)
#' @return combined dataframe with layer column, ready for mask_lines()
#' @export
stack_lines <- function(layers, z = "z", z_stack = 1, offsets = NULL) {

  z_s <- paste0(z, "s")

  normalized <- layers |>
    purrr::imap(~ .x |> dplyr::mutate(
      layer  = .y,
      !!z_s := .data[[z_s]] - min(.data[[z_s]], na.rm = TRUE)
    ))

  if (is.null(offsets)) {
    offsets <- purrr::map_dbl(normalized, ~ max(.x[[z_s]], na.rm = TRUE)) * z_stack
  }

  purrr::imap(normalized, ~ .x |> dplyr::mutate(
    !!z_s  := .data[[z_s]] + c(0, cumsum(offsets))[.y],
    y_rank  = y_rank + c(0, cumsum(purrr::map_dbl(normalized, ~ max(.x$y_rank))))[.y]
  )) |>
    dplyr::bind_rows() |>
    dplyr::arrange(y_rank)
}


#' Apply intersection masking using sliding window maximum
#'
#' Step 3 of the ridge pipeline: for each x-column, marks a point as NA if its
#' shifted value \{z\}s is too close to any of the n_lag preceding values. Uses a
#' single roll_max instead of n_lag lag columns:
#'   any(x - x_k < t) <-> x - max(x_k) < t
#'
#' @param data dataframe with xn, y_rank, \{z\}s columns (from shift_lines)
#' @param z base column name (default "z"). Reads \{z\}s, produces \{z\}n and z_rank
#' @param n_lag search range for intersection detection (number of preceding lines)
#' @param threshold minimum allowed distance between lines (m)
#' @return input dataframe with additional columns:
#'   * `z_rank`: rank of \{z\}s within each x-column (foreground = low rank)
#'   * `\{z\}n`: masked elevation — equals \{z\}s where visible, NA where occluded
#' @export
mask_lines <- function(data, z = "z", n_lag = 100, threshold = 10) {

  z_in  <- paste0(z, "s")
  z_out <- paste0(z, "n")

  data |>
    dplyr::group_by(xn) |>
    dplyr::arrange(y_rank, .by_group = TRUE) |>
    dplyr::mutate(
      z_rank  = rank(.data[[z_in]]),
      zs_rmax = roll_max_lag(.data[[z_in]], n_lag),
      zs_max  = dplyr::coalesce(dplyr::lag(zs_rmax), -Inf),
      !!z_out := dplyr::if_else(.data[[z_in]] - zs_max < threshold, NA_real_, .data[[z_in]])
    ) |>
    dplyr::select(-zs_rmax, -zs_max) |>
    dplyr::ungroup()
}

## processing ####

#' Filter ridgeline dataframe as a function of segment length
#'
#' Removes short ridge segments (fewer than `length_n` cells) and ridgelines
#' with a low proportion of visible points beyond `dist_y`.
#' @param data dataframe of processed ridgelines with zn, y_rank, y_dist columns
#' @param length_n length of ridge segments to be filtered (integer, cells)
#' @param length_x relative length used as threshold for removal of ridgeline (numeric, 0-1)
#' @param dist_y relative distance to remove ridgelines (numeric, 0-1)
#' @return filtered dataframe with short segments set to NA
#' @export
#'
filter_ridge_length <- function(
  data,
  length_n = 10,
  length_x = 5/100,
  dist_y = 0.95
) {

  # filter for ridge elements shorter than a threshold (cell number)
  data_cell <- data |> dplyr::ungroup() |> dplyr::arrange(y,x) |>
    dplyr::mutate(zl = cumsum(is.na(zn))) |>
    dplyr::group_by(zl) |> dplyr::mutate(zl_n = dplyr::n()) |> dplyr::ungroup() |>
    dplyr::mutate(zn = dplyr::if_else(zl_n <= length_n, NA_real_, zn))

  # filter for ridge lines with a low contribution
  data_line <- data |>
    dplyr::group_by(y_rank, y_dist) |>
    dplyr::summarise(xp = sum(!is.na(zn)) / dplyr::n(), .groups = "drop") |>
    dplyr::filter(xp < length_x, y_dist > dist_y)

  data_filter <- dplyr::anti_join(data_cell, data_line, by = c("y_rank", "y_dist"))

  return(data_filter)

}

#' Filter ridgeline dataframe as a function of local slope
#'
#' Computes a rolling average of the absolute slope along each ridge line and
#' sets flat segments (slope = 0) to NA.
#' @param data dataframe of processed ridgelines with zn, xn, y_rank, x_rank columns
#' @param size size of window used to compute the slope rolling average (integer, cells)
#' @return filtered dataframe with z_slope column and flat segments set to NA
#' @export

filter_ridge_slope <- function(
  data,
  size = 3
) {

  data_filter <- data |>
    dplyr::group_by(y_rank) |> dplyr::arrange(x_rank) |>
    dplyr::mutate(
      z_slope = abs(
        (zn - dplyr::lag(zn, default = 0)) / (xn - dplyr::lag(xn, default = 0))
        ),
      z_slope = RcppRoll::roll_mean(z_slope, n = size, fill = NA_real_),
      zn = dplyr::if_else(z_slope == 0, NA_real_, zn)
    ) |> dplyr::ungroup()


  return(data_filter)

}


#' Filter ridgeline dataframe as a function of rank index
#'
#' Subsets ridgelines by their y_rank using random, uniform, or strip-based sampling.
#' @param data dataframe of processed ridgelines with y_rank column
#' @param p proportion of ridgelines to keep (numeric, 0-1)
#' @param method method used to select ridges.
#'   * "random": randomly sample a proportion of ridges.
#'   * "grid": uniformly sample a proportion of ridges.
#'   * "strip": sample consecutive strips of ridges.
#' @return filtered dataframe with selected ridgelines
#' @export

filter_ridge_rank <- function(data, p, method = "grid") {

  switch(
    method,

    random = {
      dplyr::inner_join(
        data,
        data |> dplyr::distinct(y_rank) |>
          dplyr::slice_sample(prop = p)
      )
    },

    grid = {
      dplyr::inner_join(
        data,
        data |> dplyr::distinct(y_rank) |>
          dplyr::slice(
            seq(1, dplyr::n(), len = p * dplyr::n()) |>
              as.integer()
          )
      )
    },

    strip = {
      dplyr::inner_join(
        data,
        data |> dplyr::distinct(y_rank) |>
          dplyr::slice(
            sample(1:dplyr::n(), p * dplyr::n()) |>
              purrr::map(~ .x:(.x + 40)) |> purrr::flatten_int()
          )
      )
    },
    stop("Invalid `method` value")
  )
}



## generation ####

#' Process ridge lines from DEM data (3D → 2D)
#'
#' Thin wrapper around shift_lines() + mask_lines().
#' @param data dataframe with x, y, z columns
#' @param n_ridges number of ridges to select (0 = all)
#' @param n_drop number of ridges to drop from the far end
#' @param n_lag search range for intersection detection
#' @param z_shift vertical shift per rank (m)
#' @param z_threshold minimum allowed spacing between lines (m)
#' @param z_k non-linear perspective coefficient (0 = linear)
#' @return dataframe with y_rank, y_dist, dz, xn, x_rank, zs, z_rank, zn
#' @export
process_ridge <- function(
    data,
    n_ridges = 200, n_drop = 0, n_lag = 100,
    z_shift = 15, z_threshold = 10, z_k = 0) {

  data |>
    shift_lines(n_lines = n_ridges, n_drop = n_drop, shift = z_shift, k = z_k) |>
    mask_lines(n_lag = n_lag, threshold = z_threshold)
}

## rendering ####

#' Render a ridge layer with raw line aesthetics
#'
#' Creates a ggplot geom_line layer from processed ridge data.
#' @param data dataframe of processed ridge data (must contain the variables mapped to x, y, and group)
#' @param x,y,group unquoted column names mapped to x-axis, y-axis, and grouping variable
#' @param line rendering method for lines
#'   * "vector": clean lines, compatible with vector output for pen-plotting
#'   * "pencil": pencil-like texture with segments and random alpha values
#' @param size,color,alpha,lineend layer aesthetics parameters
#' @return ggplot geom_line layer
#' @export
#'
layer_ridge <- function(
  data, x = xn, y = zn, group = y_rank, line = "vector", 
  size = 0.3, color = "black", alpha = 0.8, lineend = "butt") {

  switch (
    line,

    vector = {
      ggplot2::geom_line(
        data = data,
        ggplot2::aes({{ x }}, {{ y }}, group = {{ group }}),
        linewidth = size, lineend = lineend,
        color = color, alpha = alpha, na.rm = TRUE)
    },

    # add random alpha value for line segments
    pencil = {
      ggplot2::geom_line(
        data = data |> dplyr::mutate(z_alpha = stats::rnorm(dplyr::n(), alpha, 0.2)),
        ggplot2::aes({{ x }}, {{ y }}, group = {{ group }}, alpha = z_alpha),
        linewidth = size, lineend = lineend,
        color = color, na.rm = TRUE)
    },
    stop("Invalid `line` value")
  )

}


#' Render a ridge layer with loess-smoothed line aesthetics
#'
#' Creates a ggplot geom_line layer from processed ridge data, with loess smoothing
#' applied to each ridge line via tr_recurse().
#' @param data dataframe of processed ridge data (must contain the variables mapped to x, y, and group)
#' @param x,y,group unquoted column names mapped to x-axis, y-axis, and grouping variable
#' @param span span of the smoothing function, higher values produce a smoother line (default 0.1)
#' @param n_min minimal length of ridges to apply smoothing (cells, default 100)
#' @param size,color,alpha,lineend layer aesthetics parameters
#' @param line rendering method for lines (passed to aesthetics, default "vector")
#' @return ggplot geom_line layer
#' @export
#'
layer_smooth <- function(
  data, x = xn, y = zn, group = y_rank, 
  span = 0.1, n_min = 100,
  size = 0.3, color = "black", alpha = 0.8, line = "vector", lineend = "butt"
  ) {

  # select original data
  data_raw <- data |> dplyr::select({{group}}, x = {{x}}, y = {{y}})

  # post-process paths into brush strokes
  data_tr <- data_raw |> tidyr::nest(.by = {{group}}) |>
    dplyr::mutate(tr = purrr::map(data, ~ tr_recurse(., tr_loess, tibble::lst(span, n_min)))) |>
    dplyr::select({{group}}, tr) |> tidyr::unnest(tr)

  # render plot
  ggplot2::geom_line(
    data = data_tr,
    ggplot2::aes(x, y, group = interaction({{group}}, id)),
    linewidth = size, lineend = lineend,
    color = color, alpha = alpha, na.rm = TRUE)

}


#' Compose ridge layers into a complete plot with coordinate system and cropping
#'
#' Combines layer_ridge() and optionally layer_smooth(), then applies aspect ratio cropping.
#' @param data dataframe of processed ridge data (xn, zn, y_rank columns)
#' @param method plot method: "ridge" (raw lines only) or "smooth" (raw + loess-smoothed lines)
#' @param span loess span for smoothing, higher = smoother (default 0.1, used when method = "smooth")
#' @param n_min minimum ridge length to apply smoothing in cells (default 100, used when method = "smooth")
#' @param line line rendering: "vector" (clean lines for pen-plotting) or "pencil" (textured with random alpha)
#' @param size,color,alpha,lineend line aesthetics passed to layer_ridge() and layer_smooth()
#' @param coord coordinate system: "fixed" (coord_fixed) or "free" (coord_cartesian)
#' @param ratio width:height ratio for output cropping. NULL = no cropping (default NULL)
#' @param scale scaling factor applied to y-range before cropping (default 1, used with crop = "r")
#' @param crop cropping method when ratio is set:
#'   * "x": keep y limits (max-lower to min-upper ridge), crop x-axis
#'   * "y": keep x limits, crop y-axis
#'   * "a": keep full y data range (min-lower to max-upper), crop x-axis
#'   * "r": keep full zn range, crop x-axis
#' @param expand logical, expand limits (default TRUE)
#' @param xlim,ylim manual axis limits (default NA)
#' @return ggplot object with ridge layers, coordinate system, and optional cropping
#' @export

plot_ridge <- function(
  data, method = "ridge", span = 0.1, n_min = 100, line = "vector",
  size = 0.3, color = "black", alpha = 0.8, lineend = "butt",
  coord = "fixed", ratio = NULL, scale = 1, crop = "x", expand = TRUE,
  xlim = c(NA_real_, NA_real_), ylim = c(NA_real_, NA_real_)) {

  # fixed aes args
  args_aes <- tibble::lst(size, color, alpha, line, lineend)

  # build default plot from different layers
  plot <- switch(
    method,
    ridge = {
      ggplot2::ggplot() + rlang::exec(layer_ridge, data = data, !!!args_aes)
    },
    smooth = {
      ggplot2::ggplot() +
        rlang::exec(layer_ridge, data = data, !!!args_aes) +
        rlang::exec(layer_smooth, data = data, span = span, n_min = n_min, !!!args_aes)},
    stop("Invalid `method` value")
  )

  # modify scale system
  if (line == "pencil") {plot <- plot + ggplot2::scale_alpha_identity()}

  # modify coordinate system
  plot_base <- switch(
    coord,
    fixed = {plot + ggplot2::coord_fixed(expand = expand) + ggplot2::theme_void()},
    free = {plot + ggplot2::coord_cartesian(expand = expand) + ggplot2::theme_void()},
  )
  
  # set limits as a function real x:y ratio in data.
  if (is.null(ratio)) {
    
    plot_crop <- plot_base 
    
  } else {
    
    switch (
      crop,
      
      # cut y based on x range
      "y" = {
        limits_x <- range(data$xn, na.rm = FALSE)

        limits_ymin <- c(
          tidyr::drop_na(data) |> dplyr::slice_min(y_dist) |> dplyr::pull(zn) |> max(na.rm = TRUE),
          tidyr::drop_na(data) |> dplyr::slice_max(y_dist) |> dplyr::pull(zn) |> min(na.rm = TRUE)
        )

        plot_crop <- plot_base +
            ggplot2::lims(
              x = limits_x,
              y = c(0, diff(limits_x) / ratio) + limits_ymin[1])
      }
      ,

      # cut x based on max-lower, min-upper ridge
      "x" = {

        limits_ymin <- c(
          tidyr::drop_na(data) |> dplyr::slice_min(y_dist) |> dplyr::pull(zn) |> max(na.rm = TRUE),
          tidyr::drop_na(data) |> dplyr::slice_max(y_dist) |> dplyr::pull(zn) |> min(na.rm = TRUE)
        )

        plot_crop <- plot_base +
            ggplot2::lims(
              x = c(0, diff(limits_ymin) * ratio),
              y = limits_ymin)
      },

      # cut x based on min-lower, max-upper ridge
      "a" = {

        limits_ymax <- c(
          tidyr::drop_na(data) |> dplyr::slice_min(y_dist) |> dplyr::pull(zn) |> min(na.rm = TRUE),
          tidyr::drop_na(data) |> dplyr::slice_max(y_dist) |> dplyr::pull(zn) |> max(na.rm = TRUE)
        )

        plot_crop <- plot_base +
          ggplot2::lims(
            x = c(0, diff(limits_ymax) * ratio),
            y = limits_ymax)
      },

      # cut x based on y range
      "r" = {

        y_range <- range(data$zn, na.rm = TRUE) * c(1, scale)

        plot_crop <- plot_base + ggplot2::lims(x = c(0, diff(y_range) * ratio), y = y_range)
      },
      
      stop("Invalid `crop` value")
    )
  }
  
  return(plot_crop)
  
}



# countour system ####

## rendering ####

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
  ) |>
    dplyr::mutate(
      layers = purrr::map2(
        buffer, shade,
        ~ ggplot2::geom_sf(
          data = data |> sf::st_buffer(.x), # |> st_crop(data),
          fill = NA, color = colorspace::lighten(color, .y),
          linewidth = 1/10 * scale)
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
  point <- data.frame(lon = coord["lon"], lat = coord["lat"]) |>
    sf::st_as_sf(coords = c("lon", "lat")) |>
    sf::st_set_crs(4326) |> sf::st_transform(crs = 2154)

  # define map layers with contour lines
  # TODO simplify polygons to speed up plotting with rmapshaper::ms_simplify
  # TODO crop before contour
  # TODO test isoband package to speed up calculations

  layer_shore <- data |>
    stars::st_contour(breaks = c(0, alt_min), contour_lines = TRUE) |>
    sf::st_crop(buffer_rectangle(point, ...)) %>%
    dplyr::filter(sf::st_length(.) > units::set_units(length_min,"m"))

  bb <- sf::st_bbox(layer_shore)

  switch(
    as.character(outline),

    "FALSE" = {

      breaks_major <- seq(alt_min, alt_max, by = alt_contour)

      breaks_minor <- seq(alt_min, alt_max, by = alt_contour / 2)

      # get a multipolygon object for water, used to compute waterlines
      layer_water <- data |>
        stars::st_contour(breaks = c(0, alt_min)) |>
        sf::st_buffer(dist = 0) |>
        sf::st_crop(buffer_rectangle(point, ...)) |>
        sf::st_cast("MULTIPOLYGON") |> dplyr::slice(2) |>
        sf::st_cast("POLYGON") %>%
        dplyr::filter(sf::st_area(.) > units::set_units(area_min,"m^2")) |>
        sf::st_combine()

      layer_contour_minor <- data |>
        stars::st_contour(
          breaks = breaks_minor[! breaks_minor %in% breaks_major],
          contour_lines = TRUE) |>
        sf::st_crop(buffer_rectangle(point, ...)) %>%
        dplyr::filter(sf::st_length(.) > units::set_units(length_min,"m"))

      layer_contour_major <- data |>
        stars::st_contour(
          breaks = breaks_major[! breaks_major %in% alt_min],
          contour_lines = TRUE) |>
        sf::st_crop(buffer_rectangle(point, ...)) %>%
        dplyr::filter(sf::st_length(.) > units::set_units(length_min,"m"))
      },

    "TRUE" = {},

    stop("Invalid `outline` value")
  )

  # plot contour lines

  switch(
    as.character(outline),

    "FALSE" = {

      plot_waterline <- geom_waterline(data = layer_water, n = n_water, ...)

      plot_contour_minor <- ggplot2::geom_sf(
        data = layer_contour_minor,
        color = colorspace::lighten("black", 0.6), linewidth=1/10 * scale)

      plot_contour_major <- ggplot2::geom_sf(
        data = layer_contour_major,
        color = colorspace::lighten("black", 0.4), linewidth=2/10 * scale)

      plot_shore <- ggplot2::geom_sf(
        data = layer_shore,
        color="black", linewidth = 4/10 * scale)

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
          color="black", linewidth = 4/10 * scale) +
        ggplot2::theme_void()
    },

    stop("Invalid `outline` value")
  )

  switch(
    as.character(layers),
    "TRUE" = {return(plot_layers)},
    "FALSE" = {return(plot)}
  )

}

