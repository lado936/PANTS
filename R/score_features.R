#' Score features using ezlimma and score_fcn
#' 
#' Score features using ezlimma::limma_contrasts and score_fcn
#' 
#' @inheritParams pants
#' @inheritParams ezlimma::limma_contrasts
#' @inheritParams ezlimma::limma_cor
#' @return Vector of feature scores.
#' @export

score_features <- function(object, phenotype, contrast.v, score_fcn=identity){
  toptab <- ezlimma::limma_contrasts(object=object, grp=phenotype, contrast.v=contrast.v, cols='t', add.means = FALSE)
  toptab <- data.matrix(toptab[rownames(object),])
  rownames(toptab) <- rownames(object)
  #need to coerce toptab to matrix & name score.v in case it has only one column
  score.v <- apply(as.matrix(toptab), MARGIN=1, FUN=score_fcn)
  if (!is.null(dim(score.v))){
    stop('Your score_fcn output is of length ', nrow(score.v), ' but must be of length 1.')
  }
  names(score.v) <- rownames(toptab)
  return(score.v)
}
