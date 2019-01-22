#' Transform an \code{igraph} graph into a p-step random walk kernel
#'
#' Transforms an igraph input graph into a p-step random walk kernel, using parameters \code{a} and \code{p}. This is 
#' similar to \code{netClass::calc.diffusionKernelp}, except that function takes slightly different input. The 
#' equation for the kernel is \eqn{(a * I - L)^p}. Up to scaling terms, this is equivalent to a p-step random walk on 
#' the graph with random restarts, so it is similar to the diffusion kernel, but can be calculated more cheaply (Smola & Kondor).
#'
#' @param gr graph object of class \code{igraph}.
#' @param a parameter of p-step random walk kernel, must be >= 2.
#' @param p parameter of p-step random walk kernel, must be > 0.
#' @return Laplacian kernel matrix.
#' @references Smola and Kondor, "Kernels and Regularization on Graphs" In Learning Theory and Kernel Machines, 
#' Vol. 2777 (2003), pp. 144-158, doi:10.1007/978-3-540-45167-9_12.
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
