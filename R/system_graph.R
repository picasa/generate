
#' Render point layouts with different graphs and aesthetics
#' @param data a dataframe of 2D coordinates
#' @param vars names of the coordinates to be used (unquoted vector)
#' @param graph method used to compute the graph object from the point layout.
#' * "rng": [relative neighborhood graph](https://en.wikipedia.org/wiki/Relative_neighborhood_graph),
#' roughly defines the human perception of the shape of the point set.
#' * "knn": [nearest neighbor graph](https://en.wikipedia.org/wiki/Nearest_neighbor_graph), connect k adjacent points together.,
#' * "mst" : [minimum spanning tree](https://en.wikipedia.org/wiki/Minimum_spanning_tree), connect all vertices with the minimal edge length.
#' @param k number of neighbors to account to in both methods. NA is used to calculate the true relative neighborhood graph but increase to compute time.
#' @param aes aesthetic of the rendered plot.
#' * "default": point and lines.
#' * "line": only lines.
#' * "bezier": Bezier curves.
#' @param color,width,alpha fixed aesthetic parameters for the ggplot object
#' @param coord ggplot2 coordinate system object passed to ggplot
#' @return a ggplot object
#' @export

render_graph <- function(
  data, vars = c(x,y),
  graph = "rng", aes = "default", k = 3,
  color = "black", width = 0.5, alpha=0.5, coord=NULL) {

  data <- data %>% dplyr::select({{vars}}) %>% dplyr::rename(x = 1, y = 2)

  # compute graph from point layout
  switch(
    graph,

    rng = {
      graph <- as.data.frame(data) %>%
        cccd::rng(k = k) %>% tidygraph::as_tbl_graph()
    },

    mst = {graph <- as.data.frame(data) %>%
      cccd::rng(k = k) %>% igraph::mst() %>% tidygraph::as_tbl_graph()},

    knn = {
      graph <- as.data.frame(data) %>%
        cccd::nng(k = k) %>% igraph::as.undirected(.) %>% tidygraph::as_tbl_graph()
    }
  )


  # render graph
  switch(
    aes,

    # edges as straight lines and nodes as points
    default = {

      graph %>%
        ggraph::ggraph(layout = data) +
        ggraph::geom_edge_link(edge_width = width, color = color, alpha = alpha) +
        ggraph::geom_node_point(size = width * 1.5, color = color) +
        coord + ggplot2::theme_void()
    },

    # edges as straight lines
    line = {

      graph %>%
        ggraph::ggraph(layout = data) +
        ggraph::geom_edge_link0(edge_width = width, color = color, lineend = "round") +
        coord + ggplot2::theme_void()
    },

    # edges as Bezier curves
    bezier = {

      graph %>%
        ggraph::ggraph(layout = data) +
        ggraph::geom_edge_diagonal(
          edge_width = width, color = color, alpha = alpha,
          strength = 1, n = 100, lineend = "round", linejoin = "round") +
        coord + ggplot2::theme_void()
    }
  )


}


