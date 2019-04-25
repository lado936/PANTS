#' Permute labels or indices for permutation testing
#' 
#' Permute sample labels or indices (\code{xx}) into \code{nperm} matrix rows of length 
#' \code{length(xx)} for permutation testing. The rows are not duplicates of each other, 
#' and do not contain \code{xx}. Calculations are done with \pkg{arrangements}.
#' 
#' @param xx Vector of unique labels or indices to permute. Must be of length at least 3.
#' @param freq Integer number of times to repeat any element of \code{xx} within each permutation. \code{freq=1}
#' implies sampling without replacement; \code{freq>1} implies sampling with replacement (bootstrapping). Must be
#' \code{<length(xx)}, so that a permutation is not composed of repetitions of only 1 element.
#' @inheritParams ezcombinations

ezpermutations <- function(xx, nperm, freq=length(xx)-2){
  len <- length(xx)
  stopifnot(len >= 3, len == length(unique(xx)), is.numeric(nperm), nperm >= 1, length(freq)==1, is.numeric(freq), 
            freq>0, freq<len)
  freq.v <- rep(x=freq, times=len)
  # bad.perm <- xx
  # nsamp is before subsetting
  np <- arrangements::npermutations(k = len, v = xx, freq=freq.v)
  if (nperm > np-1){
    message(nperm, " permutations requested, but only ", np-1, " available.")
    nsamp <- np
  } else {
    nsamp <- nperm + min(1, np-nperm)
  }
  pm <- arrangements::permutations(k = len, v = xx, freq=freq.v, nsample = nsamp)
  bad.ind <- which(apply(pm, MARGIN=1, FUN=function(v) all(v == xx)))
  if (length(bad.ind) > 0){
    return(pm[-bad.ind,])
  } else {
    return(pm[-sample.int(n=1:nrow(perm), size=1),])
  }
}