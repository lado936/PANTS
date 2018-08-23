#' Subset pathway inclusion matrix based on size
#' 
#' Subset pathway inclusion matrix based on number of measured features in pathway.
#' 
#' @param object A matrix-like data object containing log-ratios or log-expression values for a
#' series of samples, with rows corresponding to features and columns to samples.
#' @param Gmat Binary feature by pathway inclusion matrix, indicating which features are in which pathways.
#' @param min.size Pathways with fewer than \code{min.size} measured features in \code{object} are filtered out.
#' @return List of two: \code{Gmat} and vector \code{nfeats.per.pwy} with number of features per pathway.

subset_gmat <- function(object, Gmat, min.size){
  feats.in.pwys <- intersect(rownames(Gmat), rownames(object))
  if (length(feats.in.pwys) > 0){
    nfeats.per.pwy <- Matrix::colSums(Gmat[feats.in.pwys,,drop=FALSE] != 0)
  } else {
    nfeats.per.pwy <- setNames(object=rep(0, ncol(Gmat)), nm=colnames(Gmat))
  }
  
  if (min.size > 0){
    if (any(nfeats.per.pwy >= min.size)){
      keep.pwys <- which(nfeats.per.pwy >= min.size)
      Gmat <- Gmat[,keep.pwys,drop=FALSE]
      nfeats.per.pwy <- nfeats.per.pwy[keep.pwys]
      #clear newly empty rows of Gmat
      #use as.matrix in case ncol(Gmat)=1
      Gmat <- Gmat[rowSums(as.matrix(Gmat)) > 0,,drop=FALSE]
    } else {
      stop("No pathways are of size 'min.size'.")
    }
  }
  
  ret <- list(Gmat=Gmat, nfeats.per.pwy=nfeats.per.pwy)
  return(ret)
}