#' Get names of nodes' neighbors
#'
#' Get node names of nodes' direct neighbors
#'
#' @param graph A graph object from package \code{igraph}
#' @param nodes Node names in the graph

neighbor_nms <- function(graph, nodes){
  return(igraph::V(graph)$name[unlist(igraph::ego(graph, order=1, match(nodes, igraph::V(graph)$name)))])
}
