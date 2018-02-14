#' Expand kernel to match features in object and pathway matrix
#'
#' Expand kernel to match features in both object and pathway matrix, so that maintain these features in 
#' \code{\link{pants}}.
#' 
#' @param ker sparse matrix (of class Matrix)
#' @param Gmat sparse matrix (of class Matrix)
#' @param object A matrix-like data object containing log-ratios or log-expression values for a
#'series of samples, with rows corresponding to features and columns to samples.
#' @return sparse kernel matrix (of class Matrix)

expand_kernel <- function(ker, Gmat, object){
  #add features in both object & Gmat
  new.ker.feats <- setdiff(intersect(rownames(object), rownames(Gmat)), rownames(ker))
  if (n.new.f==0) stop("No features need to be added to the kernel.")
  n.new.f <- length(new.ker.feats)
  add2ker <- Diagonal(n=n.new.f)
  dimnames(add2ker) <- list(new.ker.feats, new.ker.feats)
  
  ker2 <- rbind(cbind(ker, Matrix(data=0, nrow=nrow(ker), ncol=n.new.f)),
                cbind(Matrix(data=0, nrow=n.new.f, ncol=ncol(ker)), add2ker))
  return(ker2)
}
