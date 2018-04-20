#'Estimate p-value from a smooth density
#'
#'Estimate p-value from a density smoothed with a non-parametric kernel density estimator or a known distribution
#'
#'@param eval.point A value where the cumulative distribution function should be evaluated
#'@param scores A vector of values from the distribution
#'@param lower.tail Logical; if TRUE (default), probabilities are P[X ≤ x] otherwise, P[X > x].
#'@param smooth.fam One of "kde" for kernel density estimation, "norm" for the normal distribution.

p_smooth_ecdf <- function(eval.point, scores, lower.tail=TRUE, smooth.fam=c("kde", "norm")){
  smooth.fam <- match.arg(smooth.fam)
  if (smooth.fam=="norm"){
    mu <- mean(scores)
    s <- stats::sd(scores)
    pv <- stats::pnorm(q=eval.point, mean = mu, sd = s, lower.tail = lower.tail)
  }#end norm
  if (smooth.fam=="kde"){
    #set 'bw' according to recommendation in ?density
    sc.dens <- stats::density(x=scores, bw="SJ")
    if (lower.tail){
      pv <- sum(sc.dens$y[sc.dens$x <= eval.point])/sum(sc.dens$y)
    } else {
      pv <- sum(sc.dens$y[sc.dens$x > eval.point])/sum(sc.dens$y)
    }
  }#end kde
  return(pv)
}
