
#' Render point layouts with different graphs and aesthetics
#' @param data a dataframe of 2D coordinates
#' @param vars names of the coordinates to be used (unquoted vector)
#' @param graph method used to compute the graph object from the point layout.
#' * "rng": [relative neighborhood graph](https://en.wikipedia.org/wiki/Relative_neighborhood_graph),
#' roughly defines the human perception of the shape of the point set.
#' * "knn": [nearest neighbor graph](https://en.wikipedia.org/wiki/Nearest_neighbor_graph), connect k adjacent points together.,
#' * "mst" : [minimum spanning tree](https://en.wikipedia.org/wiki/Minimum_spanning_tree), connect all vertices with the minimal edge length.
#' @param k number of neighbors to account to in both methods. NA is used to calculate the true relative neighborhood graph but increase to compute time.
#' @param n number of points created along the paths
#' @param strength strength of the curvature of the bend.
#' @param aes aesthetic of the rendered plot.
#' * "default": nodes as point and edges as lines
#' * "line": edges as lines
#' * "quadratic": edges as quadratic Bezier curves
#' * "cubic": edges as Cubic Bezier curves
#' * "arc": edges as arcs
#' @param lineend Line end style (round, butt, square).
#' @param color,width,alpha fixed aesthetic parameters for the ggplot object
#' @param coord ggplot2 coordinate system object passed to ggplot
#' @return a ggplot object
#' @export

render_graph <- function(
  data, vars = c(x,y),
  graph = "rng", k = 3, n = 15, strength = 0.5, aes = "default",
  lineend = "round", color = "black", width = 0.5, alpha = 1, coord = NULL) {

  data <- data |> dplyr::select({{vars}}) |> dplyr::rename(x = 1, y = 2)

  # compute graph from point layout
  switch(
    graph,

    rng = {
      graph <- as.data.frame(data) |>
        cccd::rng(k = k) |> tidygraph::as_tbl_graph()
    },

    mst = {
      graph <- as.data.frame(data) |>
        cccd::rng(k = k) |> igraph::mst() |> tidygraph::as_tbl_graph()
      },

    knn = {
      graph <- as.data.frame(data) |>
        cccd::nng(k = k) |> igraph::as.undirected() |> tidygraph::as_tbl_graph()
    },

    stop("Invalid `graph` value")
  )


  # render graph
  switch(
    aes,

    # draws edges as straight lines and nodes as points
    default = {

      graph |>
        ggraph::ggraph(layout = data) +
        ggraph::geom_edge_link(
          n = n,
          edge_width = width, color = color, alpha = alpha) +
        ggraph::geom_node_point(size = width * 1.5, color = color) +
        coord + ggplot2::theme_void()
    },

    # draws edges as straight lines
    line = {

      graph |>
        ggraph::ggraph(layout = data) +
        ggraph::geom_edge_link0(
          edge_width = width, color = color, alpha = alpha, lineend = lineend) +
        coord + ggplot2::theme_void()
    },

    # draws edges as quadratic Bezier curves
    quadratic = {

      graph |>
        ggraph::ggraph(layout = data) +
        ggraph::geom_edge_diagonal(
          n = n, edge_width = width, color = color, alpha = alpha,
          strength = strength, lineend = lineend, linejoin = lineend) +
        coord + ggplot2::theme_void()
    },

    # draws edges as cubic bezier curves
    cubic = {

      graph |>
        ggraph::ggraph(layout = data) +
        ggraph::geom_edge_bend(
          n = n, edge_width = width, color = color, alpha = alpha,
          strength = strength, lineend = lineend) +
        coord + ggplot2::theme_void()

    },


    # draws edges as arcs
    arc = {
      graph |>
        ggraph::ggraph(layout = data) +
        ggraph::geom_edge_arc(
          #aes(alpha = ..index..),
          edge_width = width, color = color, alpha = alpha,
          n = n, strength = strength, lineend = lineend) +
        coord + ggplot2::theme_void()
    },

    stop("Invalid `aes` value")
  )


}


