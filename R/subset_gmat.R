#' Subset pathway inclusion matrix based on size
#' 
#' Subset pathway inclusion matrix based on number of measured features in pathway.
#' 
#' @inheritParams pants
#' @inheritParams ezlimma::limma_contrasts
#' @inheritParams ezlimma::roast_contrasts
#' @return List of two: \code{Gmat} and vector \code{nfeats.per.pwy} with number of features per pathway.

subset_gmat <- function(object, Gmat, min.nfeats){
  feats.in.pwys <- intersect(rownames(Gmat), rownames(object))
  if (length(feats.in.pwys) > 0){
    nfeats.per.pwy <- Matrix::colSums(Gmat[feats.in.pwys,,drop=FALSE] != 0)
  } else {
    nfeats.per.pwy <- stats::setNames(object=rep(0, ncol(Gmat)), nm=colnames(Gmat))
  }
  
  if (min.nfeats > 0){
    if (any(nfeats.per.pwy >= min.nfeats)){
      keep.pwys <- which(nfeats.per.pwy >= min.nfeats)
      Gmat <- Gmat[,keep.pwys,drop=FALSE]
      nfeats.per.pwy <- nfeats.per.pwy[keep.pwys]
      #clear newly empty rows of Gmat
      #use as.matrix in case ncol(Gmat)=1
      Gmat <- Gmat[rowSums(as.matrix(Gmat)) > 0,,drop=FALSE]
    } else {
      stop("No pathways are of size 'min.nfeats'.")
    }
  }
  
  ret <- list(Gmat=Gmat, nfeats.per.pwy=nfeats.per.pwy)
  return(ret)
}