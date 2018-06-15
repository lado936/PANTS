#' Transform an \code{igraph} graph into a p-step random walk kernel
#'
#' Transforms an igraph input graph into a p-step random walk kernel, using parameters \code{a} and \code{p}.
#' This is similar to \code{netClass::calc.diffusionKernelp}, except that function takes
#' slightly different input. The equation for the kernel is (a * I - L)^p.
#'
#' @param gr graph object of class \code{igraph}.
#' @param a parameter of p-step random walk kernel.
#' @param p parameter of p-step random walk kernel.
#' @return Laplacian kernel matrix.
#' @export

#future work: can describe dimensions in description
graph2kernel <- function(gr, a=2, p=1){
  stopifnot(a >= 2, p > 0)
  # agrees w/ p-step random walk kernel in Gao et al.
  # L = graph.laplacian(graph=gr, normalized = TRUE)
  if (!igraph::is_simple(gr)){
    message('igraph::is_simple(gr) is FALSE, so applying igraph::simplify.')
    gr <- igraph::simplify(gr)
  }
  
  L = igraph::laplacian_matrix(graph=gr, normalized = TRUE, sparse=TRUE)
  I = Matrix::Diagonal(n=ncol(L))
  R = mat_pow(a * I - L, p)
  dimnames(R) <- dimnames(L)
  return(R)
}
