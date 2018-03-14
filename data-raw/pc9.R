library(devtools)
library(PANTS)

pc9 <- read.table('B:/annotations/pwy_commons/PathwayCommons9.All.hgnc.sif')
pc9 <- clean_pwycommons_sif(pc9, rm.ids=c("", "CHEBI:15377"))
pc9 <- pc9[,c(1,3)]
devtools::use_data(pc9, overwrite = TRUE)
