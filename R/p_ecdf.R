#'Estimate p-value from simulations
#'
#'Estimate p-value by comparing a score to its simulations
#'
#'@param eval.v A vector of values where the cumulative distribution function should be evaluated
#'@param scores.mat A matrix of values from the distribution, where each column is a different simulation, 
#'and \code{rownames(scores.mat)} corresponds to \code{names(eval.v)}.
#'@param alternative A character string specifying the alternative hypothesis
#'@return Vector of p-values of \code{length(eval.v)}.

p_ecdf <- function(eval.v, scores.mat, alternative=c("two.sided", "less", "greater")){
  stopifnot(length(eval.v)==nrow(scores.mat), names(eval.v)==rownames(scores.mat), ncol(scores.mat) > 1)
  alternative <- match.arg(alternative)
  nsim <- ncol(scores.mat)
  
  #add one to numerator and denominator to avoid p-values of zero, which aren't correct
  if (alternative == "greater"){
    pv <- (rowSums(eval.v < scores.mat) + 0.5*rowSums(eval.v == scores.mat) + 1)/(nsim+1)
  }
  if (alternative == "less"){
    pv <- (rowSums(eval.v > scores.mat) + 0.5*rowSums(eval.v == scores.mat) + 1)/(nsim+1)
  }
  #https://stats.stackexchange.com/questions/140107/p-value-in-a-two-tail-test-with-asymmetric-null-distribution
  if (alternative == "two.sided"){
    gr <- rowSums(eval.v > scores.mat) + 0.5*rowSums(eval.v == scores.mat)
    less <- rowSums(eval.v < scores.mat) + 0.5*rowSums(eval.v == scores.mat)
    min.v <- apply(cbind(less, gr), MARGIN=1, FUN=min)
    pv <- (2*min.v + 1)/(nsim+1)
  }
  return(pv)
}
