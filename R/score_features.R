#' Score features using ezlimma and score_fcn
#' 
#' Score features using \code{\link[ezlimma]{limma_contrasts}} or \code{\link[ezlimma]{limma_cor}} and 
#' then applying \code{score_fcn} to moderated t-statistics. Does not currently accept \code{type=="mediation"}.
#' 
#' @inheritParams pants
#' @inheritParams ezlimma::limma_contrasts
#' @inheritParams ezlimma::limma_cor
#' @return Named vector of feature scores.
#' @export

score_features <- function(object, phenotype=NULL, type=c("contrasts", "correlation"), 
                           contrast.v=NULL, design=NULL, score_fcn=abs){
  type <- match.arg(type)
  if (type=="mediation") stop("Mediation not currently supported.", call. = FALSE)
  if (type=="contrasts"){
    toptab <- ezlimma::limma_contrasts(object=object, grp=phenotype, contrast.v=contrast.v, design=design, 
                                       cols="t", add.means = FALSE)
  } else {
    toptab <- ezlimma::limma_cor(object=object, phenotype = phenotype, design=design, cols="t")
  }
  toptab <- data.matrix(toptab[rownames(object),, drop=FALSE])
  rownames(toptab) <- rownames(object)
  # need to coerce toptab to matrix & name score.v in case it has only one column
  score.v <- apply(as.matrix(toptab), MARGIN=1, FUN=score_fcn)
  if (!is.null(dim(score.v))){
    stop("Your score_fcn output is of length ", nrow(score.v), " but must be of length 1.", call. = FALSE)
  }
  names(score.v) <- rownames(toptab)
  return(score.v)
}
