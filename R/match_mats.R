#' Expand kernel to match features in object and pathway matrix
#'
#' Expand kernel to match features in both object and pathway matrix, so that maintain these features in 
#' \code{\link{pants}}.
#' 
#' @param score.mat A matrix-like object with scores from original data and possibly permutations with rows corresponding 
#' to features and columns to simulations.
#' @param ker sparse matrix (of class Matrix).
#' @param Gmat Feature-by-pathway inclusion matrix, indicating which features are in which pathways.
#' @param score.impute Value to impute missing scores. If zero, \code{score.mat} is sparser, saving computing time.
#' @details There must be some overlap between the rownames of \code{score.mat}, \code{ker}, & \code{Gmat}.
#' @return A list with elements \code{score.mat}, \code{ker}, and \code{Gmat} after matching.
#' @export

match_mats <- function(score.mat, ker, Gmat, score.impute=0){
  stopifnot(length(intersect(rownames(score.mat), rownames(ker))) > 0, 
            length(intersect(rownames(score.mat), rownames(Gmat))) > 0)
  
  all.feats <- unique(c(rownames(score.mat), rownames(ker), rownames(Gmat)))
  
  venn.mat <- cbind(all.feats %in% rownames(score.mat), all.feats %in% rownames(ker), all.feats %in% rownames(Gmat))
  rownames(venn.mat) <- all.feats
  vm.rs <- rowSums(venn.mat)
  #keep features in 2 of 3 mats
  keep.feats <- names(vm.rs)[vm.rs >= 2]
  
  #add features to scores as zero matrix at bottom
  #don't want to reorder this since it's the most dense
  sc.feats <- rownames(score.mat)
  new.sc.feats <- setdiff(keep.feats, sc.feats)
  keep.sc.feats <- intersect(sc.feats, keep.feats)
  new.sc.n <- length(new.sc.feats)
  score.mat <- score.mat[keep.sc.feats,,drop=FALSE]
  if (new.sc.n > 0){
    score.mat <- rbind(score.mat, Matrix::Matrix(score.impute, nrow=new.sc.n, ncol=ncol(score.mat)))
    rownames(score.mat) <- c(keep.sc.feats, new.sc.feats)
  }
  
  #add features to ker as diagonal mat in lower right
  ker.feats <- rownames(ker)
  new.ker.feats <- setdiff(keep.feats, ker.feats)
  new.ker.n <- length(new.ker.feats)
  if (new.ker.n > 0){
    ker <- rbind(cbind(ker, Matrix::Matrix(data=0, nrow=nrow(ker), ncol=new.ker.n)),
                  cbind(Matrix::Matrix(data=0, nrow=new.ker.n, ncol=ncol(ker)), Matrix::Diagonal(n=new.ker.n)))
    colnames(ker) <- rownames(ker) <- c(ker.feats, new.ker.feats)
  }
  ker <- ker[rownames(score.mat), rownames(score.mat)]
  
  #add features to G as zero matrix
  g.feats <- rownames(Gmat)
  new.g.feats <- setdiff(keep.feats, g.feats)
  new.g.n <- length(new.g.feats)
  if (new.g.n > 0){
    Gmat <- rbind(Gmat, Matrix::Matrix(0, nrow=new.g.n, ncol=ncol(Gmat)))
    rownames(Gmat) <- c(g.feats, new.g.feats)
  }
  Gmat <- Gmat[rownames(score.mat),,drop=FALSE]
  
  ret <- list(score.mat=score.mat, ker=ker, Gmat=Gmat)
  return(ret)
}
