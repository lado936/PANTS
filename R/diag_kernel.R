#' Make diagonal kernel matrix
#' 
#' Make diagonal kernel matrix to run Pants without smoothing.
#' 
#' @inheritParams pants
#' @inheritParams ezlimma::limma_contrasts
#' @details \code{rownames(object)} and \code{rownames(Gmat)} should have some overlap. \code{object} is only used to extract \code{rownames(object)}.
#' @return A diagonal matrix.

# no need to keep features not in both objects when there's no smoothing.
diag_kernel <- function(object, Gmat){
  stopifnot(length(intersect(rownames(object), rownames(Gmat))) > 0)
  int.rows <- intersect(rownames(object), rownames(Gmat))
  dk <- Matrix::Diagonal(n=length(int.rows))
  dimnames(dk) <- list(int.rows, int.rows)
  return(dk)
}