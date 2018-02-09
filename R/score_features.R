#'Score features using ezlimma and score_fcn
#'
#'Score features using ezlimma::limma_contrasts and score_fcn
#'
#'@param object A matrix-like data object containing log-ratios or log-expression values for a
#'series of samples, with rows corresponding to features and columns to samples.
#'@param phenotypes.v A vector of phenotypes of strings the same length as number of samples in \code{object}.
#'If the vector is named, the names must match the column names of \code{object}.
#'@param contrasts.v A named vector of constrasts. The constrasts must refer to the phenotypes
#'in \code{phenotypes.v}. Their order defines the order they are passed to \code{score_fcn}.
#'@param score_fcn A function that transforms the t-statistics from the contrasts.
#'Set to \code{identity} so it is the trivial identity function returning its argument.
#'Default is absolute value. Its input must be a vector of same length as number of elements in \code{contrasts.v}.
#'Its output must be a scalar (i.e. a vector of length one).
#'@return Vector of feature scores.

score_features <- function(object, phenotypes.v, contrasts.v, ker, Gmat, score_fcn){
  limma.cols <- c('P.Value', 'adj.P.Val', 't', 'logFC')
  toptab <- limma_contrasts(object=object, grp=phenotypes.v, contrasts.v=contrasts.v, cols=limma.cols)
  toptab.ss <- toptab[,paste0(names(contrasts.v), ".t")]
  #need to coerce toptab.ss to matrix & name score.v in case it has only one column
  score.v <- apply(as.matrix(toptab.ss), MARGIN=1, FUN=score_fcn)
  if (!is.null(dim(score.v))){
    stop('Your score_fcn\'s output is of length ', ncol(toptab.ss), ' but it should be of length 1.')
  }
  names(score.v) <- rownames(toptab)
  return(score.v)
}
