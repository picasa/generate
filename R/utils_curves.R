
# mapping functions ####

#' Map a continuous variable to 3 discrete levels
#' @param x numeric vector for input values
#' @param i numeric vector of length 2 for breaks
#' @param o numeric vector of length 3 for output values
#' @return a numeric vector
#' @export

f_square <- function(x, i, o) {
  x <- scales::rescale(x, to=c(0,1))
  dplyr::case_when(x > i[2] ~ o[3], x > i[1] ~ o[2], TRUE ~ o[1])
}


#' Exponential function
#' @param x numeric vector for input values
#' @param k,a,b exponential rate, scaling, and asympotic parameters
#' @return a numeric vector
#' @export

f_exp <- function(x, k = 1, a = 1, b = 0) {
  x <- scales::rescale(x, to=c(0,1))
  a * exp(k * x) + b
}
