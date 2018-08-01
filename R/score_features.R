#' Score features using ezlimma and score_fcn
#' 
#' Score features using ezlimma::limma_contrasts and score_fcn
#' 
#' @param object A matrix-like data object containing log-ratios or log-expression values for a
#' series of samples, with rows corresponding to features and columns to samples.
#' @param phenotype.v A vector of phenotypes of strings the same length as number of samples in \code{object}.
#' If the vector is named, the names must match the column names of \code{object}.
#' @param contrast.v A named vector of constrasts. The constrasts must refer to the phenotypes
#' in \code{phenotype.v}. Their order defines the order they are passed to \code{score_fcn}.
#' @param score_fcn A function that transforms the t-statistics from the contrasts. \code{identity} is 
#' the trivial identity function returning its argument. It must accept a vector of \code{length(contrast.v)}, 
#' and its output must be a scalar.
#' @return Vector of feature scores.
#' @export

score_features <- function(object, phenotype.v, contrast.v, score_fcn=identity){
  toptab <- ezlimma::limma_contrasts(object=object, grp=phenotype.v, contrast.v=contrast.v, cols='t', add.means = FALSE)
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
