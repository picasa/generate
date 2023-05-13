
# mapping functions ####

#' Map a continuous variable to 3 discrete levels
#' @param x numeric vector for input values
#' @param i numeric vector of length 2 for breaks
#' @param o numeric vector of length 3 for output values
#' @param scale Boolean, if TRUE x input values are scaled (scales::rescale)
#' @return a numeric vector
#' @export
#'
f_square <- function(x, i, o, scale = FALSE) {
  if (scale == TRUE) x = scales::rescale(x, to=c(0,1)) else x
  dplyr::case_when(x > i[2] ~ o[3], x > i[1] ~ o[2], TRUE ~ o[1])
}


#' Exponential function
#' @param x numeric vector for input values
#' @param k,a,b exponential rate, scaling, and asymptotic parameters
#' @param scale Boolean, if TRUE x input values are scaled (scales::rescale)
#' @return a numeric vector
#' @export
#'
f_exp <- function(x, k = 1, a = 1, b = 0, scale = FALSE) {

  if (scale == TRUE) {
    x = suppressWarnings(scales::rescale(x, to=c(0,1)))
    } else x

  a * exp(k * x) + b
}

#' Sigmoid function
#' @param x numeric vector for input values
#' @param k,a,b rate, scaling, and asymptotic parameters
#' @param scale Boolean, if TRUE x input values are scaled (scales::rescale)
#' @export
#'
f_sig <- function(x, k = 2, a = -2, b = 1, scale = FALSE) {

  if (scale == TRUE) {
    x = suppressWarnings(scales::rescale(x, to = c(0,1)))
    } else x

  a / (1 + exp(k * x)) + b

}
