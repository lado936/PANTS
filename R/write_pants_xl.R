#' Write Excel XLSX file with links to CSVs
#' 
#' Create directory; write CSV files with stats from \code{feat.tab} for all features in each pathway, and the top
#' 3 features that impact each pathway (even if they're outside the pathway); and write an Excel XLSX file using 
#' \code{writexl} that links to the CSVs.
#'
#' @inheritParams pants
#' @inheritParams plot_pwy
#' @inheritParams ezlimma::write_top_xl
#' @details It's checked that \code{rownames(score.mat)==names(eval.v)}.
#' @return A matrix with two columns containing z-scores (larger is more significant) & p-values with 
#' \code{nrow = length(eval.v)}.

write_pants_xl <- function(score.v, pwy.tab, feat.tab, Gmat, ker, alternative=c("two.sided", "less", "greater"), 
                           annot=NULL, name=NA, n.toptabs=Inf){
  
  feat.lst <- lapply(colnames(Gmat), FUN=function(pwy){
    select_ntop(score.v=score.v, Gmat=Gmat, pwy=pwy, ker=ker, alternative=alternative, ntop=3)
  })
  names(feat.lst) <- colnames(Gmat)
  
  if (!is.null(annot)){
    feature.stats.ann <- data.frame(signif(feat.tab, 3), annot[rownames(feat.tab), ])
  } else {
    feature.stats.ann <- data.frame(signif(feat.tab, 3))
  }
  
  ezlimma:::write_top_xl(pwy.tab=pwy.tab, feat.lst=feat.lst, feat.tab=feature.stats.ann, name=name, n.toptabs=n.toptabs)
}