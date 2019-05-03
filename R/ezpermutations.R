#' Resample group labels with replicates for permutation testing
#' 
#' Resample unique sample or group labels (unique elements of \code{xx}) into \code{nperm} matrix rows of length 
#' \code{length(xx)} for permutation testing. The rows are not duplicates of each other, 
#' do not contain \code{xx}, and have each element of \code{unique(xx)} to appear at least once.
#' Calculations are done with \pkg{arrangements}.
#' 
#' @param xx Vector of labels or indices to resample. Must be of length at least 2.
#' @param nperm Number of resamples to return.
#' @param freq Integer number of times to repeat each element of \code{names(table(xx))} within each resample.
#' Can be a vector where \code{length(freq)==length(table(xx))} or of length 1, which is recycled into a vector.
#' Represents resampling without replacement if it's \code{table(xx)}. For resampling with replacement (bootstrapping),
#' \code{length(xx)-length(table(xx))+1} allows each element of \code{unique(xx)} to appear at least once 
#' in each resample.
#' @inheritParams arrangements::permutations
#' @details Number of returned resamples may be less than \code{nperm} if not enough unique resamples are available.
#' @return List

# freq is independent of actual n.per.grp with bootstrapping
# freq can represent replace, so don't need replace
# assume alternative is one-sided, as in pants, so only rm perms w/ missing groups
ezpermutations <- function(xx, nperm, freq=length(xx)-length(unique(xx))+1){
  stopifnot(length(xx) >= 2, is.numeric(nperm), nperm >= 1, is.numeric(freq), freq>0, freq<length(xx),
            length(freq)==1 || length(freq)==length(unique(xx)))
  ta <- table(xx)
  
  freq.v <- if (length(freq)==1){
    rep(x=freq, times=length(ta))
  } else {
    freq
  }
  
  # need replace=FALSE (default) so it respects freq
  # R's max int is 2B, so use try() for integer overflow errors
  np <- 10**9
  try(np <- min(arrangements::npermutations(k = length(xx), v = names(ta), freq=freq.v), 10**9), silent=TRUE)
  if (np==0) stop("No such permutations available.", call. = FALSE)
    
  ns1 <- min(np, 10**4)
  round2 <-  ifelse(ns1 == np, FALSE, TRUE)
  # nsample argument --> duplicates!
  ind1 <- sample.int(n=np, size=ns1)
  p1 <- arrangements::permutations(k = length(xx), v = names(ta), freq=freq.v, index = ind1, layout = "list")
  rej <- sapply(p1, FUN=function(v){
      all(v == xx) ||  length(unique(v)) < length(ta)
  })

  if (!round2){
    p1 <- p1[!rej]
    if (length(p1) > nperm) p1 <- p1[sample.int(n=length(p1), size = nperm)]
    return(p1)
  } else {
    prob.nonrej <- sum(!rej)/ns1
    # n to select, then sample from successes
    ns2 <- opt_binom_n(p=prob.nonrej, nperm = nperm)
    ind2 <- sample.int(n=np, size=ns2)
    p2 <- arrangements::permutations(k = length(xx), v = names(ta), freq=freq.v, index = ind2, layout = "list")
    rej <- sapply(p2, FUN=function(v){
      all(v == xx) ||  length(unique(v)) < length(ta)
    })
    p2 <- p2[!rej]
    p2 <- p2[sample.int(n=length(p2), size=min(length(p2), nperm))]
    return(p2)
  }
}
