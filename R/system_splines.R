
# sample ####

#' Sample n letters in the R letters set.
#' @param n number of sampled elements
#' @param x set of letters to sample from
#' @return a concatenated character string
#' @export
#'
sample_letters <- function(n = 3, x = letters) {
  sample(x, n, replace = TRUE) |> paste0(collapse = "")
}


# generate ####


#' Generate text sequences with various properties.
#' @param seed random seed for the character sequence.
#' @param method method used for sequence generation
#'  * lipsum, generate long sentences latin-based text, with the option to subset the text into smaller elements (scale argument).
#'  * sentence, draw short sentences of comparable length from the Revised List of Phonetically Balanced Sentences [Harvard Sentences](https://www.cs.columbia.edu/~hgs/audio/harvard.html)
#'  * random, recurvively generate sentences from random length, with words of given size range by randomly sampling letters.
#' @param scale scale of the returned elements, for the lipsum method. Control how the generated text is
#' divided into word, sentence, or paragraph.
#' @param n_paragraph,n_sentence,n_word,n_chr number of elements at each organizational level, i.e. number of paragraphs, sentences, words, and characters. The last two levels can be specified as a vector of length to sample in.
#' @return a character string.
#' @export
#'
gen_sequence <- function(
    seed = NULL, method = "lipsum", scale = "sentence",
    n_paragraph = 1, n_sentence = 1, n_word = 3:5, n_chr = 3:8
    ) {

  # set seed for text sequence
  if (!is.null(seed)) set.seed(seed)

  switch (

    method,

    # generate paragraphs and extract the first n sentences
    lipsum = {

      string <- stringi::stri_rand_lipsum(
        n_paragraphs = n_paragraph, start_lipsum = FALSE)

      seq <- switch (
        scale,

        "word" = {
          paste0(string, collapse = " ") |>
            stringr::word(1:n_word[1], sep = stringr::fixed(" "))},

        "sentence" = {
          paste0(string, collapse = " ") |>
            stringr::word(1:n_sentence[1], sep = stringr::fixed(". "))},

        "paragraph" = {string},

        stop("Invalid `scale` value")
      )

    },

    # draw sentences from https://www.cs.columbia.edu/~hgs/audio/harvard.html
    sentence = {
      seq <- sentences[sample(length(sentences), n_sentence)]
    },

    # generate sentences containing random words within a given size range
    random = {

      seq <- purrr::map(1:n_sentence,
                 ~  purrr::map(sample(n_chr, sample(n_word, 1), replace = TRUE),
                        ~ sample_letters(n = .)) |> paste(collapse = " ")
                 ) |> purrr::list_c()
    },

    stop("Invalid `method` value")
  )

  return(seq)

}

#' Define a map between characters and generated glyphs
#' @description Generate a set of control points for a collection of glyphs mapped to characters. This initial map is then modified by merging n glyphs into one and adding jitter.
#' @param seed random seed for the character map.
#' @param n number of letters in the character map. The total length of the character map is n + 3 (addition of ".", ",", "?")
#' @param n_control number of control points for each glyph
#' @param n_tall number of tall glyphs
#' @param size_tall radius of the set of control points for tall glyphs
#' @param n_merge number of glyphs concatenated in the modified map
#' @param n_variation number of variations of each glyphs in the map
#' @param jitter amount of jitter added to each variations
#' @param scale,rotation scale (0-1) and rotation (radian) of the set of control points
#' @return a list-column dataframe with a character and layout column (set of control points).
#' @export
#'
gen_charmap <- function(
    seed, n = 26, n_control = 4, n_tall = 4, size_tall = 4,
    n_merge = 3, n_variation = 10, jitter = 1/5,
    scale = 0.5, rotation = -pi/6) {

  # set seed for charmap
  if (!missing(seed)) set.seed(seed)

  # create an initial character map
  data_map <- tibble::tibble(
    pattern = 1:(n + 3),
    character = c(letters[1:n], ".", ",", "?"),
    r = dplyr::case_when(
      rbinom((n + 3), 1, p = n_tall/(n + 3)) == 0 ~ 1,
      TRUE ~ size_tall)
  ) |>
    dplyr::mutate(layout = purrr::map(
      r, ~ layout_ellipse(n = n_control, r = ., scale_x = scale, a = rotation)))

  # add a base layout for whitespace
  layout_blank <- dplyr::tibble(n = 1, x = 0, y = 0, group = NA)
  data_blank <- dplyr::tibble(character = " ", layout = list(layout_blank), variation = 1)

  # modify the character map by merging n glyphs into one and adding jitter
  data_glyphs <- data_map |>
    dplyr::mutate(
      word = purrr::map_chr(1:n(), ~ sample_letters(n = n_merge, x = letters[1:n])),
      layout = purrr::map(word, ~ layout_glyph(., data_map, shift = 1))) |>
    tidyr::crossing(variation = 1:n_variation) |>
    dplyr::mutate(
      layout = purrr::map(
        layout,
        ~ dplyr::mutate(..1, across(x:y, ~ jitter(., amount = jitter))))) |>
    dplyr::bind_rows(data_blank)

  return(data_glyphs)

}

# layout ####

#' Create a concatenated set of control points (glyph) from a sequence of characters.
#' @description One glyph is created from n characters, defined by individual sets of control points.
#' @param seq a sequence of characters (string)
#' @param map a character map generated by `gen_charmap()`
#' @param shift horizontal shift when concatenating individual sets of control points
#' @return a dataframe of concatenated sets of control points
#' @export
#'
layout_glyph <- function(seq, map, shift = 1) {

  tibble::tibble(character = stringr::str_split(seq, "")[[1]]) |>
    dplyr::left_join(map, by = "character") |> tidyr::drop_na() |>
    dplyr::mutate(d = seq(0, by = shift, length = dplyr::n())) |>
    dplyr::mutate(
      layout = purrr::map2(layout, d, ~ dplyr::mutate(..1, x = x + ..2))
      ) |>
    tidyr::unnest(layout)
}


#' Layout a glyph set as a function of a character string and a character map.
#' @description Emulates text by juxtaposing glyphs, and adding individual variation.
#' @param seq a character sequence.
#' @param map a character map generated by `gen_charmap()`
#' @param cut maximum number of glyphs on a line or columns
#' @param scale scaling parameter for individual glyphs
#' @param noise random spacing between characters (sd) and words (mean, sd)
#' @param orientation writing direction (e.g. right to left, top to bottom)
#' @return a dataframe with coordinates of glyphs.
#' @export

layout_sequence <- function(
    seq, map, cut = 80, scale = c(2.5, 5), noise = c(0, 0), orientation = "lrtb") {

  # split input string to characters
  seq_chr <- seq |> stringr::str_to_lower() |> stringr::str_split("")

  # map string characters to glyph layouts, spaces and non-match creates NULL layout
  # select one variation per character.
  string <- dplyr::tibble(glyph = seq_chr[[1]]) |>
    dplyr::mutate(position = 1:dplyr::n(), word = cumsum(glyph == " ")) |>
    dplyr::left_join(
      map |> dplyr::select(glyph = character, variation, layout),
      dplyr::join_by(glyph), relationship = "many-to-many") |>
    dplyr::slice_sample(n = 1, by = position) |>
    dplyr::filter(!purrr::map_lgl(layout, is.null))

  # layout glyphs in 2D by shifting their coordinates
  layout <- string |>
    dplyr::mutate(
      col = ((position - 1) %% cut),
      row = ((position - 1) %/% cut)
    ) |>
    dplyr::mutate(e_c = stats::rnorm(dplyr::n(), 0, noise[1])) |>
    dplyr::mutate(e_w = stats::rnorm(1, noise[2], noise[2]), .by = word) |>
    dplyr::mutate(layout = purrr::pmap(
      list(layout, col, row, e_w, e_c),
      ~ ..1 |> dplyr::mutate(
        x0 = x + (..2 * scale[1]) + (..4 * scale[1]) + ..5,
        y0 = y - (..3 * scale[2])))) |>
    tidyr::unnest(layout)

  layout <- switch(
    orientation,
    "lrtb" = {layout |> dplyr::mutate(x = x0, y = y0)},
    "rltb" = {layout |> dplyr::mutate(x = -x0, y = y0)},
    "tbrl" = {layout |> dplyr::mutate(x = y0, y = -x0)},
    "tblr" = {layout |> dplyr::mutate(x = -y0, y = -x0)},
    stop("Invalid `orientation` value")
  )

  return(layout)

}


#' Layout a glyph set as a function of a character string and a character map.
#' @description Emulates text by juxtaposing glyphs, and adding individual variation.
#' @param seq a character sequence.
#' @param map a character map generated by `gen_charmap()`
#' @param cut maximum number of glyphs on a line or columns
#' @param scale scales between glyphs (x) and lines (y)
#' @param noise random spacing between characters (sd) and words (mean, sd)
#' @param spacing relative space between paragraphs
#' @param orientation writing direction (e.g. right to left, top to bottom)
#' @return a dataframe with coordinates of glyphs.
#' @export

layout_paragraph <- function(
    seq, map, cut = 80, scale = c(2.5, 5), noise = c(0, 0), spacing = 1, orientation = "lrtb") {

  # layout strings as glyph sequences
  layout <- dplyr::tibble(seq = seq) |>
    dplyr::mutate(
      layout = purrr::map(
        seq, ~ layout_sequence(
          seq = ..1, map = map, scale = scale, noise = noise,
          cut = cut, orientation = orientation))
    )

  # layout sequences as paragraphs
  p_space <- switch(
    orientation,
    "lrtb" = {c(0, spacing)}, "rltb" = {c(0, spacing)},
    "tbrl" = {c(- spacing, 0)}, "tblr" = {c(spacing, 0)},
    stop("Invalid `orientation` value")
  )

  layout <- layout |>
    dplyr::mutate(
      section = 0:(n()-1),
      line = ceiling(stringr::str_length(seq) / cut) |> cumsum(),
      shift = dplyr::lag(line, default = 0) * scale[2],
      layout = purrr::map2(
        layout, shift,
        ~ dplyr::mutate(..1, x = ..2 * p_space[1] + x, y = - ..2 * p_space[2] + y))
    )

  return(layout)

}

# render ####

#' Render spline curves from control points and grouping indices.
#' @param data a dataframe with x and y columns defining control points, and *group* and *glyph* columns defining grouping structure.
#' @param type Either 'clamped' (default) or 'open'. Ensures the spline starts and ends at the terminal control points.
#' @param n number of points generated for the spline curve.
#' @param color,width,alpha arguments passed to `geom_bspline()`
#' @param coord coordinate system passed to `ggplot()`, default to coord_fixed()
#' @return a ggplot object
#' @export
#'
render_spline <- function(
    data, type = "clamped", n = 100,
    color = "black", width = 0.5, alpha = 1, coord = ggplot2::coord_fixed()) {

  # glyphs mapped to whitespace are not drawed, but plot limits account for them
  plot <- ggplot2::ggplot() +
    ggforce::geom_bspline(
      data = data |> dplyr::filter(glyph != " "),
      ggplot2::aes(x, y, group = group),
      lineend = "round", type = type, n = n,
      color = color, linewidth = width, alpha = alpha) +
    coord + ggplot2::lims(x = range(data$x), y = range(data$y)) +
    ggplot2::theme_void()

  return(plot)

}

