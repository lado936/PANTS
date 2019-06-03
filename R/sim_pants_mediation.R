#' Simulate \code{pants} with mediation to estimate size & power
#' 
#' Simulate \code{pants} with mediation to estimate size (effect=0) & power (effect>0).
#' 
#' @param effect.v Numeric vector of log fold-changes or percent of phentotypes to add.
#' @inheritParams ezlimma::sim_barfield
#' @inheritParams pants
#' @seealso \code{\link{sim_pants}}

# contr
# test mult pwys for efficiency
sim_pants_mediation <- function(Gmat, exposure, effect.v=c(0, 0.2), alpha=0.05, nsim=10**3,
                      nperm=10**3, seed=1, verbose=TRUE, ker=NULL, ncores=1){
  prop.sig.mat <- matrix(NA, nrow=nsim, ncol=length(effect.v), 
                         dimnames=list(paste0("sim", 1:nsim), paste0("eff_", effect.v)))
  phenotype <- exposure + rnorm(n=length(exposure), sd=sd(exposure)/2)
  
  set.seed(seed)
  for (sim in 1:nsim){
    for (ev in effect.v){
      obj.test <- matrix(stats::rnorm(n=nrow(Gmat)*length(phenotype)), ncol=length(phenotype), 
                         dimnames=list(rownames(Gmat), names(phenotype)))
      if (ev > 0){
          obj.test <- t(t(obj.test) + exposure + ev*phenotype)
      }
      res <- pants(object=obj.test, exposure=exposure, phenotype=phenotype, Gmat=Gmat, nperm=nperm, type="mediation",
                   seed=sample(10**5, size = 1), ker=ker, ncores=ncores)
      prop.sig.mat[sim, paste0("eff_", ev)] <- mean(res$pwy.stats[, "p"] < alpha)
    }
    if (sim %% 100 == 0) cat("sim: ", sim, "\n")
  }
  (prop.sig.mat <- rbind(avg=colMeans(prop.sig.mat), prop.sig.mat))
}