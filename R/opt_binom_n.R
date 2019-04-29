#' Find minimal binomial N that ensures \code{ezpermutations} obtains \code{nperm} unique permutations
#' 
#' Find minimal binomial N that ensures \code{ezpermutations} obtains \code{nperm} unique permutations. 
#' Let \eqn{X~binom(N, p=prob.nonrej)}, so \eqn{P(X>nperm)<\epsilon} <--> \eqn{Np-8\sqrt{Npq}-nperm=0}
#' <--> \eqn{\sqrt{N}=(\frac{8\sqrt{pq}+\sqrt{64pq + 4 \cdot p \cdot nperm}}{2p})} by quadratic formula in \eqn{\sqrt{N}}.
#' 
#' @param p Probablity a resample is successful.
#' @inheritParams ezpermutations

opt_binom_n <- function(p, nperm){
  stopifnot(length(p)==1, length(nperm)==1, p>=0, p<=1, nperm>=2)
  if (p < 0.01) p <- 0.01
  # p=1 doesn't cause error, but only yields nperm, whereas we want more to account for var(p)
  if (p > 0.99) p <- 0.99
  ceiling( (( 8*sqrt(p*(1-p)) + sqrt(64*p*(1-p) + 4*p*nperm) ) / (2*p) )^2 )
}
