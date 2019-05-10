#' Simulate \code{pants} to estimate size & power
#' 
#' Simulate \code{pants} to estimate size (effect=0) & power (effect>0) with \code{score_fcn=identity} vs. 
#' H1: "greater". Does not currently support \code{type=="mediation"}.
#' 
#' @param effect.v Numeric vector of log fold-changes or percent of phentotypes to add.
#' @inheritParams ezlimma::sim_barfield
#' @inheritParams pants

# contr
# test mult pwys for efficiency
sim_pants <- function(Gmat, phenotype, type=c("contrasts", "correlation", "mediation"), 
                      effect.v=c(0, 0.2), alpha=0.05, nsim=10**3,
                      nperm=10**3, seed=1, verbose=TRUE, ker=NULL, ncores=1){
  type <- match.arg(type)
  if (type=="mediation") stop("Mediation not currently supported.", call. = FALSE)
  stopifnot(!is.null(names(phenotype)))
  
  prop.sig.mat <- matrix(NA, nrow=nsim, ncol=length(effect.v), 
                         dimnames=list(paste0("sim", 1:nsim), paste0("eff_", effect.v)))
  if (type=="contrasts"){
    contrast.v <- c(vs=paste(unique(phenotype)[2], unique(phenotype)[1], sep="-"))
  } else {
    contrast.v <- NULL
  }
  
  set.seed(seed)
  for (sim in 1:nsim){
    for (ev in effect.v){
      obj.test <- matrix(stats::rnorm(n=nrow(Gmat)*length(phenotype)), ncol=length(phenotype), 
                         dimnames=list(rownames(Gmat), names(phenotype)))
      if (ev > 0){
        if (type=="contrasts"){
          obj.test[, phenotype == unique(phenotype)[2]] <- obj.test[, phenotype == unique(phenotype)[2]] + ev
        } else {
          obj.test <- obj.test + ev*matrix(phenotype, nrow=nrow(obj.test), ncol=ncol(obj.test), byrow = TRUE)
        }
      }
      res <- pants(object=obj.test, Gmat=Gmat, phenotype=phenotype, type=type, score_fcn=identity,
                   contrast.v=contrast.v, nperm=nperm, seed=sample(10**5, size = 1), ker=ker, ncores=ncores)
      prop.sig.mat[sim, paste0("eff_", ev)] <- mean(res$pwy.stats[, "p"] < alpha)
    }
    if (sim %% 100 == 0) cat("sim: ", sim, "\n")
  }
  (prop.sig.mat <- rbind(avg=colMeans(prop.sig.mat), prop.sig.mat))
}