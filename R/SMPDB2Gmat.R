#'Transform SMPDB protein and/or metabolite pathway data frames into a pathway matrix
#'
#'Transform SMPDB protein and/or metabolite pathway data frames, read in as CSV files from http://smpdb.ca/downloads,
#'into a pathway matrix that \code{PANTS} can use to compute pathway scores.
#'
#'@param smpdb.prot data frame of proteins per pathway from SMPDB
#'@param smpdb.met data frame of metabolites per pathway from SMPDB
#'@param exclude.pwy.subj vector of subjects (or types) of pathways to exclude
#'@return A numeric Matrix of features (proteins and/or metabolites) as rows and pathways as columns, indicating pathway membership
#'@export

SMPDB2Gmat <- function(smpdb.prot, smpdb.met, exclude.pwy.subj=NA, sc=FALSE){
  stopifnot(c('Pathway.Subject', 'Gene.Name') %in% colnames(smpdb.prot),
            c('Pathway.Type', 'ChEBI.ID') %in% colnames(smpdb.met))

  if (!is.na(exclude.pwy.subj[1])){
    smpdb.prot <- smpdb.prot[!(smpdb.prot$Pathway.Subject %in% exclude.pwy.subj),]
    smpdb.met <- smpdb.met[!(smpdb.met$Pathway.Type %in% exclude.pwy.subj),]
  }
  
  #remove analytes w/ no ID
  smpdb.prot <- smpdb.prot[smpdb.prot$Gene.Name!="",]
  smpdb.met <- smpdb.met[!is.na(smpdb.met$ChEBI.ID),]
  
  smpdb.prot$feat.name <- smpdb.prot$Gene.Name
  smpdb.met$feat.name <- paste0('CHEBI:', smpdb.met$ChEBI.ID)

  smpdb.tab <- rbind(smpdb.prot[,c('Pathway.Name', 'feat.name')], smpdb.met[,c('Pathway.Name', 'feat.name')])

  smpdb.tab <- smpdb.tab[!is.na(smpdb.tab$feat.name) & smpdb.tab$feat.name!='',]

  all.feats <- sort(unique(smpdb.tab$feat.name))
  all.pwys <- sort(unique(smpdb.tab$Pathway.Name))

  Gmat <- Matrix(0, nrow=length(all.feats), ncol=length(all.pwys), dimnames = list(all.feats, all.pwys))
  for (i in 1:nrow(smpdb.tab)){
    Gmat[smpdb.tab[i,2], smpdb.tab[i,1]] <- 1
  }
  return(Gmat)
}
