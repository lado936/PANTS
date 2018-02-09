#' Get names of nodes' neighbors
#'
#' Get node names of nodes' direct neighbors
#'
#' @param graph A graph object from package \code{igraph}
#' @param nodes Nodes in the graph

neighbor_nms <- function(graph, nodes){
  if (packageVersion("igraph") < "1.0.0") {
    return(V(graph)$name[unlist(igraph::neighborhood(graph, order=1, match(nodes, V(graph)$name)))])
  } else {
    return(V(graph)$name[unlist(igraph::ego(graph, order=1, match(nodes, V(graph)$name)))])
  }
}
