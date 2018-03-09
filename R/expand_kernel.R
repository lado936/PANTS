#' Expand kernel to match features in object and pathway matrix
#'
#' Expand kernel to match features in both object and pathway matrix, so that maintain these features in 
#' \code{\link{pants}}.
#' 
#' @param ker sparse matrix (of class Matrix)
#' @param object.feat.nms feature names from object.
#' @return sparse kernel matrix (of class Matrix)
#' @export

expand_kernel <- function(ker, object.feat.nms){
  #add features in both object & Gmat
  new.ker.feats <- setdiff(object.feat.nms, rownames(ker))
  n.new.f <- length(new.ker.feats)
  if (n.new.f==0){ stop("No features need to be added to the kernel.") }
  add2ker <- Diagonal(n=n.new.f)
  dimnames(add2ker) <- list(new.ker.feats, new.ker.feats)
  
  ker2 <- rbind(cbind(ker, Matrix(data=0, nrow=nrow(ker), ncol=n.new.f)),
                cbind(Matrix(data=0, nrow=n.new.f, ncol=ncol(ker)), add2ker))
  colnames(ker2) <- rownames(ker2) <- c(rownames(ker), new.ker.feats)
  return(ker2)
}
