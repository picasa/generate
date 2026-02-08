
# exports ####

#' Save R plot with presets file formats
#' @param plot R plot object
#' @param file file name without extension
#' @param path file path without trailing backslash
#' @param format file format
#' * png, print quality bitmap output (400 dpi, lossless)
#' * jpg, default bitmap output (218 dpi, compressed)
#' * snap, screen quality output (150 dpi, default path)
#' * svg, vector format
#' @param size numeric vector for image width and height in mm, default to A5
#' @param dpi image resolution in dpi (default 218 for 1:1 display at 50% zoom
#'   with 218 dpi screen and 2x scale factor)
#' @param bg background color passed to ggsave function
#' @param cmd bash vector processing command to execute after SVG export (vector method only)
#' @export
#'
save_plot <- function(
    plot, file, path = "R/figures",
    size = c(148, 210), format = "jpg", dpi = NULL,
    bg = "#FEFAEE", cmd = NULL
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
        dpi = ifelse(is.null(dpi), 218, dpi), width = size[1], height = size[2],
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
      
      if (!is.null(cmd)) system(cmd)
    },

    stop("Invalid `format` value")
  )

}
