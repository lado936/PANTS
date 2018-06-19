#' Make diagonal kernel matrix
#' 
#' Make diagonal kernel matrix to run Pants without smoothing.
#' 
#' @param object A matrix-like data object containing log-ratios or log-expression values for a
#' series of samples, with rows corresponding to features and columns to samples.
#' @param Gmat The feature by pathway inclusion matrix, indicating which features are in which pathways.
#' @details \code{rownames(object)} and \code{rownames(Gmat)} should have some overlap.
#' @return A diagonal matrix.

#no need to keep features not in both objects when there's no smoothing.
diag_kernel <- function(object, Gmat){
  stopifnot(length(intersect(rownames(object), rownames(Gmat))) > 0)
  int.rows <- intersect(rownames(object), rownames(Gmat))
  dk <- Matrix::Diagonal(n=length(int.rows))
  dimnames(dk) <- list(int.rows, int.rows)
  return(dk)
}