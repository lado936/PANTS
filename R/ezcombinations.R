#' Combine group labels for permutation testing
#' 
#' Combine unique group labels (unique elements of \code{xx}) into \code{nperm} matrix rows of length 
#' \code{length(xx)} for permutation testing. The rows are not duplicates of each other, 
#' do not contain \code{xx}, and have each element of \code{unique(xx)} to appear at least once.
#' Calculations are done with \pkg{arrangements}.
#' 
#' @param xx Vector of labels or indices to resample. Must be of length at least 3.
#' @param nperm Number of resamples to return.
#' @param freq Integer number of times to repeat each element of \code{names(table(xx))} within each resample.
#' Can be a vector where \code{length(freq)==length(table(xx))} or of length 1, which is recycled into a vector.
#' Represents resampling without replacement if it's \code{table(xx)}. For resampling with replacement (bootstrapping),
#' \code{length(xx)-length(table(xx))+1} allows each element of \code{unique(xx)} to appear at least once 
#' in each resample.
#' @details Number of returned resamples may be less than \code{nperm} if not enough resamples were available. 

# freq is independent of actual n.per.grp with bootstrapping
# freq can represent replace, so don't need replace
# this isn't right: doesn't account for symmetry = f(xx, contr.v, alternative)
ezcombinations <- function(xx, nperm, freq=length(xx)-length(unique(xx))+1){
  len <- length(xx)
  ta <- table(xx)
  stopifnot(len >= 3, is.numeric(nperm), nperm >= 1, is.numeric(freq), freq>0, freq<len,
            length(freq)==1 || length(freq)==length(unique(xx)))
  freq.v <- if (length(freq)==1){
    rep(x=freq, times=length(ta))
  } else {
    freq
  }
  
  nc <- arrangements::ncombinations(k = len, v = names(ta), freq=freq.v)
  nsamp <- min(ifelse(alt.type=="two.sided", 4*nperm, 2*nperm), nc)
  pm <- arrangements::permutations(k = len, v = names(ta), freq=freq.v, nsample = nsamp)
  rej.ind <- which(apply(pm, MARGIN=1, FUN=function(v){
      all(v == xx) ||  length(unique(v)) < length(ta)
  }))
  if (length(rej.ind) > 0){ pm <- pm[-rej.ind,] }
  if (nrow(pm) > nperm){
    pm <- pm[sample(1:nrow(pm), size = nperm)]
  }
  pm
}