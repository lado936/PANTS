#' Make diagonal kernel matrix
#' 
#' Make diagonal kernel matrix to run Pants without smoothing.
#' 
#' @param object.rownames Rownames of \code{object}. Must be non-duplicated and non-empty.
#' @param Gmat.rownames Rownames of \code{Gmat}. Must be non-duplicated and non-empty.
#' @details \code{object.rownames} and \code{Gmat.rownames} should have some overlap.
#' @return A diagonal matrix.

# no need to keep features not in both objects when there's no smoothing.
diag_kernel <- function(object.rownames, Gmat.rownames){
  stopifnot(length(intersect(object.rownames, Gmat.rownames)) > 0)
  int.rows <- intersect(object.rownames, Gmat.rownames)
  dk <- Matrix::Diagonal(n=length(int.rows))
  dimnames(dk) <- list(int.rows, int.rows)
  return(dk)
}