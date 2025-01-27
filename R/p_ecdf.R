#' Estimate p-value from simulations
#'
#' Estimate p-value by comparing a score to its permutations.
#'
#' @param eval.v A vector of values where the cumulative distribution function should be evaluated.
#' @param alternative A character string specifying the alternative hypothesis.
#' @inheritParams match_mats
#' @inheritParams ezlimma::roast_contrasts
#' @details It's checked that \code{rownames(score.mat)==names(eval.v)}. If a p-value is 1, it results in a z-score of 
#' -Inf, which can produce an error in downstream analysis, so we calculate a new p-value as 1 - 10^(-6).   
#' @return A matrix with two columns containing z-scores (larger is more significant) & p-values with 
#' \code{nrow = length(eval.v)}.
#' @references Phipson B, Smyth GK. Permutation P-values should never be zero: calculating exact P-values when 
#' permutations are randomly drawn. Stat Appl Genet Mol Biol. 2010;9:Article39. doi: 10.2202/1544-6115.1585.

# (b+1)/(m+1) is conservative, so I previously used 0.5*(eval.v==score.mat)
p_ecdf <- function(eval.v, score.mat, alternative=c("greater", "two.sided", "less")){
  stopifnot(length(eval.v)==nrow(score.mat), names(eval.v)==rownames(score.mat), ncol(score.mat) > 1)
  alternative <- match.arg(alternative)
  nsim <- ncol(score.mat)
  
  # add one to numerator and denominator to avoid p-values of zero, which aren't correct
  if (alternative == "greater"){
    pv <- (rowSums(score.mat >= eval.v) + 1)/(nsim+1)
    if (any(pv == 1)){ pv[pv==1] <- 1 - 10**(-6) }
    zv <- stats::qnorm(p=pv, lower.tail = FALSE)
  }
  if (alternative == "less"){
    pv <- (rowSums(score.mat <= eval.v) + 1)/(nsim+1)
    if (any(pv == 1)){ pv[pv==1] <- 1 - 10**(-6) }
    zv <- stats::qnorm(p=pv, lower.tail = TRUE)
  }
  # https://stats.stackexchange.com/questions/140107/p-value-in-a-two-tail-test-with-asymmetric-null-distribution
  if (alternative == "two.sided"){
    gr <- rowSums(score.mat <= eval.v)
    less <- rowSums(score.mat >= eval.v)
    min.v <- apply(cbind(less, gr), MARGIN=1, FUN=min)
    #1st mult by 2, then give correction for pv=0; other way could give pv>1
    pv <- (2*min.v + 1)/(nsim + 1)
    if (any(pv == 1)){ pv[pv==1] <- 1 - 10**(-6) }
    wm.v <- apply(cbind(less, gr), MARGIN=1, FUN=which.min)
    zv <- stats::qnorm(p=pv/2, lower.tail = wm.v-1)
  }
  return(cbind(z=zv, p=pv))
}