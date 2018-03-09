#' Transform a data frame edge list into a p-step random walk kernel
#'
#' Transforms an edge list into a p-step random walk kernel. The edge list can come from
#' columns 1 and 3 of a SIF (Simple Interaction Format) file, or can otherwise
#' contain pairs of nodes connected in the graph. This function calls
#' \code{\link{gr2kernel}} to calculate the kernel matrix.
#'
#' @param edge.lst Edge-list
#' @param a parameter of p-step random walk kernel, a>=2
#' @param p parameter of p-step random walk kernel
#' @return p-step random walk kernel matrix
#' @export

edgeList2kernel <- function(edge.lst, a=2, p=1){
  net <- simplify(graph_from_edgelist(as.matrix(edge.lst), directed = FALSE))
  #V(net)$name <- toupper(V(net)$name)
  ker <- gr2kernel(net, a=a, p=p)
  # dimnames(ker) <- list(toupper(rownames(ker)), toupper(colnames(ker)))
  return(ker)
}
