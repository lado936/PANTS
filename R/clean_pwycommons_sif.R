#' Clean Pathway Commons SIF file
#'
#' Apply a few curation steps to "clean" Pathway Commons Simple Interaction Format (SIF) file, 
#' based on manual curation of Pathway Commons 8.
#'
#' @param pc.sif SIF file from Pathway Commons
#' @param rm.ids vector of analyte IDs to remove. Default \code{CHEBI:15377} corresponds to water.
#' @return SIF file
#' @export

clean_pwycommons_sif <- function(pc.sif, rm.ids="CHEBI:15377"){
  stopifnot(ncol(pc.sif) >= 3)
  #yields 2 conjugate acids that should be switched
  pc.sif <- apply(pc.sif, 2, FUN=function(v){ gsub('CHEBI:36261$', 'CHEBI:9410', gsub('CHEBI:60008$', 'CHEBI:37998', v))  })
  #rm rows with CHEBI:15377=water
  rm.rows <- which(apply(pc.sif, MARGIN=1, FUN=function(v){
    length(grep(paste(paste0("^", rm.ids, "$"), collapse="|"), v)) >0
  }))
  pc.sif <- pc.sif[-rm.rows,]
  pc.sif <- pc.sif[-which(duplicated(pc.sif[,c(1,3)])),]
  return(pc.sif)
}
