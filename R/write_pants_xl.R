#' Write Excel XLSX file with links to CSVs
#' 
#' Create directory; write CSV files with stats from \code{feat.tab} for all features in each pathway, and the top
#' features that impact each pathway (even if they're outside the pathway); and write an Excel XLSX file using 
#' \code{writexl} that links to the CSVs.
#' 
#' @param feat.tab Table of feature (e.g. gene) statistics and annotation.
#' @inheritParams pants
#' @inheritParams select_ntop_per_pwy
#' @inheritParams ezlimma::write_linked_xl
#' @return Invisibly, a list with two components:
#' \describe{
#'    \item{\code{pwys.xl}}{Data frame representing written Excel file}
#'    \item{\code{pwy.csvs}}{List of data frames representing each written pathway CSV file}
#'  }

# req kernel: ok, since fcn not exported
# feat.tab may have score column if spit out from pants/pants_hitman, but need not
write_pants_xl <- function(zscores, pwy.tab, feat.tab, Gmat, ker, name, ntop=5){
  stopifnot(row(pwy.tab) > 0, nrow(feat.tab) > 0, ncol(ker) == nrow(zscores), colnames(ker) == rownames(zscores),
            all(rownames(zscores) %in% rownames(feat.tab)), !is.null(ker), ncol(ker) == nrow(Gmat), 
            rownames(ker)==colnames(ker), !is.null(name))
  if (!requireNamespace("writexl", quietly = TRUE)){
    stop("Install 'writexl' package.", call. = FALSE)
  }
  
  xp <- ezlimma:::xl_pwys(pwy.tab=pwy.tab)
  # need to clean pwy names in Gmat & ker to match
  colnames(Gmat) <- substr(ezlimma::clean_filenames(colnames(Gmat)), 1, 150)
  
  # should provide ordered nodes
  feat.lst <- lapply(rownames(xp), FUN=function(pwy){
    select_ntop_per_pwy(zscores=zscores, Gmat=Gmat, pwy=pwy, ker=ker, ntop=ntop)
  })
  names(feat.lst) <- rownames(xp)
  
  if (!is.na(name)){
    if (file.exists(name)) unlink(name, recursive = TRUE)
    dir.create(name)
    dir.create(paste0(name, "/pathways"))
  }
  names(feat.lst) <- ezlimma::clean_filenames(names(feat.lst))
  csv.lst <- list()
  for (pwy in rownames(xp)){
    fl.tmp <- feat.lst[[pwy]]
    ft <- data.frame(in_pwy=fl.tmp$in.pwy, impact=fl.tmp$impact, feat.tab[fl.tmp$node,], stringsAsFactors = FALSE)
    ft[,setdiff(colnames(ft), "in_pwy")] <- ezlimma::df_signif(tab=ft[,setdiff(colnames(ft), "in_pwy")], digits=3)
    if (!is.na(name)) utils::write.csv(ft, paste0(name, "/pathways/", pwy, ".csv"))
    csv.lst[[pwy]] <- ft
  }
  
  xp.out <- xp
  xp.out[,-1] <- signif(x=xp.out[,-1], digits=3)
  if (!is.na(name)) writexl::write_xlsx(x=xp.out, path = paste0(name, "/", name, ".xlsx"))
  
  ret <- list(pwys.xl=xp, pwy.csvs=csv.lst)
  return(invisible(ret))
}