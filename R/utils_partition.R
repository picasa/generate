# functions for partitioning (returns fractions) and indexing (returns index for tables)

# Partition ####

#' Partition a unit into k parts working with 1/n th fractions combinations
#' @param k, number of parts (1:5), vector
#' @param n, partition fraction (1-5), integer
#' @param sample, if TRUE (default) sample a random partition fitting the conditions,
#' else enumerate distinct partitions
#' @return a random vector of fraction summing to 1
#' @export
partition <- function(k = 1:3, n = 6, sample = TRUE) {

  # list regular and irregular k-partitions of 1/n fractions, without permutations
  list_partitions <- switch(
    n,
    {list(1)},
    {list(c(1/2, 1/2))},
    {list(
      c(2/3, 1/3),
      rep(1/3, 3)
    )},
    {list(
      c(3/4, 1/4),
      c(2/4, 1/4, 1/4),
      rep(1/4, 4)
    )},
    {list(
      c(4/5, 1/5), c(3/5, 2/5),
      c(3/5, 1/5, 1/5), c(2/5, 2/5, 1/5),
      c(2/5, 1/5, 1/5, 1/5),
      rep(1/5, 5)
    )},
    {list(
      rep(3/6, 2), c(4/6, 2/6),
      rep(2/6, 3), c(3/6, 2/6, 1/6),
      c(3/6, 1/6, 1/6, 1/6), c(2/6, 2/6, 1/6, 1/6),
      c(2/6, 1/6, 1/6, 1/6, 1/6)
    )}
  )

  # compute all distinct permutations for a given partition, including 1-partition
  table_partition <- dplyr::tibble(cut = c(1, list_partitions)) |>
    dplyr::mutate(
      n = purrr::map_int(cut, ~ length(..1)),
      part = purrr::map(
        cut, ~ dplyr::tibble(p = combinat::permn(..1)) |> dplyr::distinct())
    )

  # sample a possible partition or enumerate distinct partitions
  if (isTRUE(sample)) {
    l <- table_partition |> dplyr::filter(n %in% k) |> tidyr::unnest(part) |>
      dplyr::slice_sample(n = 1) |> dplyr::pull(p) |> purrr::list_c()
  } else {
    l <- table_partition |> dplyr::filter(n %in% k) |>
      tidyr::unnest(part) |> dplyr::pull(p)
  }

  return(l)

}

# Index ####

#' Round down the maximum value of x to the nearest multiple of accuracy
#' @param x, numeric value or vector
#' @param accuracy, integer number defining the rounding limit
#' @return integer value
#' @export
round_near <- function(x, accuracy) {
  (max(x) %/% accuracy) * accuracy
}

#' Create group index by partitioning a vector.
#' @param x vector to index
#' @param k, number of group, integer
#' @param length, length of each group
#' @param method discretization method.
#' * fixed, fixed size groups, remainder creates an incomplete group
#' * quantiles, breaks are defined by quantile probabilities as a function of group number and noise
#' * random, breaks are randomly sampled in the vector range as a function of group number.
#' * hclust, are defined by 1D hierarchical clustering.
#' @return a vector of indices of the same length as x
#' @export

index_group <- function(x, k = NULL, length = NULL, method) {

  # no indexing with one group
  if (k == 1) {
    index = rep(1, len = length(x))
  } else {
    switch (

        method,

        # fixed interval number or width (x - 1) %/% width + 1
        "fixed" = {
          index <- ggplot2::cut_interval(
            dplyr::dense_rank(x), n = k, length = length, labels = FALSE)
        },

        # quantiles + noise
        "quantiles" = {
          probs <- seq(0, (k-2)/(k-1), len = k-1)
          noise <- stats::runif(k-1, 1/(4*k), 1/k)
          breaks <- c(min(x), stats::quantile(x, p = probs + noise), max(x)) |> unique()
          index <- ggplot2::cut_interval(x, n = k-1, breaks = breaks, labels = FALSE)
        },

        # breaks randomly sampled
        "random" = {
          breaks = c(min(x), sample(min(x):max(x), k-1), max(x)) |> sort() |> unique()
          index <- ggplot2::cut_interval(x, n = k-1, breaks = breaks, labels = FALSE)
        },

        "hclust" = {
          index <- x |> stats::dist() |> stats::hclust() |> stats::cutree(k = k)
        },

        stop("Invalid `method` value")
      )
  }
  return(index)
}
