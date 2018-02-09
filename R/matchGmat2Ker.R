#' Match pathway matrix features to kernel matrix features
#'
#' Match pathway matrix (Gmat) features to kernel matrix features, so that we can multiply these matrices
#' later.
#'
#' @param Gmat sparse matrix (of class Matrix)
#' @param ker sparse matrix (of class Matrix)
#' @return sparse Gmat matrix (of class Matrix)
#' @export

matchGmat2Ker <- function(Gmat, ker){
  setdiff.sym <- setdiff(rownames(ker), rownames(Gmat))
  #match to kernel by imputing missing Gmat values as 0, indicating that they don't directly contribute to pwy score
  #rBind() is deprecated
  Gmat.match <- rbind(Gmat, Matrix(data=0, nrow=length(setdiff.sym), ncol=ncol(Gmat),
                                   dimnames=list(setdiff.sym, colnames(Gmat))))
  #lose some genes unique to pwys, but they may not be in data anyway
  Gmat.match <- Gmat.match[rownames(ker),]
  return(Gmat.match)
}
