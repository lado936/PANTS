#' Expand graph to match features in pathway matrix
#'
#' Expand graph to match features in pathway matrix, so that maintain these features in 
#' \code{\link{pants}}.
#' 
#' @param gr Interaction network of class \code{igraph}.
#' @param Gmat Pathway membership matrix, can be sparse matrix from package \code{Matrix}.
#' @return Interaction network of class \code{igraph}.
#' @details Nodes in \code{Gmat} but not \code{gr} are added as isolated nodes to \code{gr} so that their scores
#' can be incorporated in \code{\link{pants}} and plotted in \code{\link{plot_pwy}}.
#' @export

expand_graph <- function(gr, Gmat){
  new.v <- setdiff(rownames(Gmat), V(gr)$name)
  if (length(new.v) > 0){
    gr <- igraph::add_vertices(graph=gr, nv=length(new.v), name=new.v)
  } else {
    warning("No new nodes to add.")
  }
  return(gr)
}
