library(devtools)
library(PANTS)

pc9 <- read.table('B:/annotations/pwy_commons/PathwayCommons9.All.hgnc.sif')
pc9 <- sif2edgelist(pc9, rm.ids="CHEBI:15377")

stopifnot(ncol(pc9) == 2)
stopifnot(!apply(pc9, 1, FUN=is.unsorted))

devtools::use_data(pc9, overwrite = TRUE)
