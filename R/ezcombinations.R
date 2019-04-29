#' Resample group labels with replicates for permutation testing
#' 
#' Resample unique group labels (unique elements of \code{xx}) into \code{nperm} matrix rows of length 
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
# assume alternative is one-sided, as in pants, so only rm perms w/ missing groups
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
  
  np <- arrangements::npermutations(k = len, v = names(ta), replace=TRUE, freq=freq.v)
  # iff give nsample, get duplicates!
  ns1 <- min(np, 10**4)
  round2 <-  ifelse(ns1 == np, FALSE, TRUE)
  p1 <- arrangements::permutations(k = len, v = names(ta), replace=TRUE, freq=freq.v, nsample = ns1)
  rej <- apply(p1, MARGIN=1, FUN=function(v){
      all(v == xx) ||  length(unique(v)) < length(ta)
  })

  if (!round2){
    p1 <- p1[!rej,]
    if (nrow(p1) > nperm) p1 <- p1[sample.int(1:nrow(p1), size = nperm),]
    return(p1)
  } else {
    prob.nonrej <- sum(!rej)/ns1
    # X~binom(N=ns2, p=prob.nonrej); P(X>nperm)<1% --> P(ns2*p-3*sqrt(ns2*p*q>nperm))<1%
    ns2 <- ceiling( (nperm/(8*prob.nonrej*sqrt(prob.nonrej * (1-prob.nonrej))))^(2/3) )
    p2 <- arrangements::permutations(k = len, v = names(ta), replace=TRUE, freq=freq.v, nsample = ns2)
    rej <- apply(p2, MARGIN=1, FUN=function(v){
      all(v == xx) ||  length(unique(v)) < length(ta)
    })
    p2 <- p2[!rej,]
    return(p2)
  }
}