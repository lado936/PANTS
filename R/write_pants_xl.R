#' Write Excel XLSX file with links to CSVs
#' 
#' Create directory; write CSV files with stats from \code{feat.tab} for all features in each pathway, and the top
#' 3 features that impact each pathway (even if they're outside the pathway); and write an Excel XLSX file using 
#' \code{writexl} that links to the CSVs.
#'
#' @inheritParams pants
#' @inheritParams plot_pwy
#' @inheritParams ezlimma::write_linked_xl

# req kernel: ok, since fcn not exported
# feat.tab may have score column if spit out from pants/pants_hitman, but need not
write_pants_xl <- function(score.v, pwy.tab, feat.tab, Gmat, ker, name, alternative=c("two.sided", "less", "greater"),
                           ntop=5){
  stopifnot(!is.null(names(score.v)), is.finite(score.v), nrow(pwy.tab) > 0, nrow(feat.tab) > 0, 
            !is.null(ker), ncol(ker) == nrow(Gmat), ncol(ker) == length(score.v), colnames(ker) == names(score.v), 
            !is.null(name), rownames(ker)==colnames(ker))
  if (!requireNamespace("writexl", quietly = TRUE)){
    stop("Install 'writexl' package.", call. = FALSE)
  }
  
  xp <- ezlimma:::xl_pwys(pwy.tab=pwy.tab)
  #need to clean pwy names in Gmat & ker to match
  colnames(Gmat) <- substr(ezlimma::clean_filenames(colnames(Gmat)), 1, 150)
  
  #should provide ordered nodes
  feat.lst <- lapply(rownames(xp), FUN=function(pwy){
    select_ntop(score.v=score.v, Gmat=Gmat, pwy=pwy, ker=ker, alternative=alternative, ntop=ntop)
  })
  names(feat.lst) <- rownames(xp)
  
  if (file.exists(name)) unlink(name, recursive = TRUE)
  
  dir.create(name)
  dir.create(paste0(name, '/pathways'))
  names(feat.lst) <- ezlimma::clean_filenames(names(feat.lst))
  for(pwy in rownames(xp)){
    fl.tmp <- feat.lst[[pwy]]
    ft <- data.frame(in_pwy=fl.tmp$in.pwy, impact=fl.tmp$impact, feat.tab[fl.tmp$node,], stringsAsFactors = FALSE)
    ft[,setdiff(colnames(ft), "in_pwy")] <- ezlimma::df_signif(tab=ft[,setdiff(colnames(ft), "in_pwy")], digits=3)
    utils::write.csv(ft, paste0(name, '/pathways/', pwy, '.csv'))
  }
  
  xp.out <- xp
  xp.out[,-1] <- signif(x=xp.out[,-1], digits=3)
  writexl::write_xlsx(x=xp.out, path = paste0(name, "/", name, ".xlsx"))
  
  return(invisible(xp))
}