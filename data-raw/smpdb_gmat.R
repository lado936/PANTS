library(devtools)
library(PANTS)
options(stringsAsFactors = FALSE)
setwd("//jdcfs1/cores/bioinformatics")
source('fcns/PANTS/R/SMPDB2Gmat.R')

smpdb.prot <- read.csv('annotations/smpdb/smpdb_proteins.csv')
smpdb.met <- read.csv('annotations/smpdb/smpdb_metabolites.csv')
smpdb_gmat <- SMPDB2Gmat(smpdb.prot=smpdb.prot, smpdb.met=smpdb.met, 
                         exclude.pwy.subj = c('Disease', 'Drug Action', 'Physiological', 'Signaling', 'Drug Metabolism'))
devtools::use_data(smpdb_gmat, overwrite = TRUE)
