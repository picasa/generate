
#' Save R plot with presets file formats
#' @param plot R plot object
#' @param file file name without extension
#' @param path file path without trailing backslash
#' @param format file format
#' * png, print quality bitmap output (400 dpi, lossless)
#' * jpg, default bitmap output (200 dpi, compressed)
#' * snap, screen quality output (150 dpi, default path)
#' * svg, vector format
#' @param size numeric vector for image width and height in mm, default to A5
#' @param dpi resolution (dpi)
#' @param bg background color passed to ggsave function
#' @export
#'
save_plot <- function(
    plot, file, path = "R/figures",
    size = c(148, 210), format = "jpg", dpi = NULL, bg = "#FEFAEE"
) {

  switch(
    format,

    png = {
      ggplot2::ggsave(
        plot, file = glue::glue("{path}/{file}.png"),
        dpi = ifelse(is.null(dpi), 400, dpi), width = size[1], height = size[2],
        scale = 1, units="mm", bg = bg)
    },

    jpg = {
      ggplot2::ggsave(
        plot, file = glue::glue("{path}/{file}.jpg"),
        dpi = ifelse(is.null(dpi), 200, dpi), width = size[1], height = size[2],
        scale = 1, units="mm", bg = bg)
    },

    snap = {
      ggplot2::ggsave(
        plot, file = glue::glue("{path}/snapshots/{file}.jpg"),
        dpi = ifelse(is.null(dpi), 150, dpi), width = size[1], height = size[2],
        scale = 1, units="mm", bg = bg)
    },

    svg = {
      ggplot2::ggsave(
        plot, file = glue::glue("{path}/{file}.svg"),
        width = size[1], height = size[2], scale=1, units="mm")
    },

    stop("Invalid `format` value")
  )

}
