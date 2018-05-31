#' Transform a data frame edge list into an \code{igraph} graph
#'
#' Transforms an edge list into an \code{igraph} graph. The edge list can come from
#' columns 1 and 3 of a SIF (Simple Interaction Format) file, or can otherwise
#' contain pairs of nodes connected in the graph.
#'
#' @param edge.lst Edge-list
#' @return An \code{igraph} graph
#' @export
#' @importFrom igraph graph_from_edgelist simplify

edgelist2graph <- function(edge.lst){
  net <- igraph::simplify(igraph::graph_from_edgelist(as.matrix(edge.lst), directed = FALSE))
  return(net)
}
