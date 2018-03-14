library(devtools)
library(PANTS)

smpdb.prot <- read.csv('B:/annotations/smpdb/smpdb_proteins.csv')
smpdb.met <- read.csv('B:/annotations/smpdb/smpdb_metabolites.csv')
smpdb_gmat <- SMPDB2Gmat(smpdb.prot=smpdb.prot, smpdb.met=smpdb.met, exclude.pwy.subj = c("Drug Metabolism"))
devtools::use_data(smpdb_gmat, overwrite = TRUE)
