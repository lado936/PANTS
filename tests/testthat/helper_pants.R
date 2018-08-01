library(testthat)
library(vdiffr)

#sq w/ all nodes connected except a <-> c; a <-> d repeated, as in sif
el <- rbind(t(combn(letters[1:4], 2))[-2,], c("a", "d"))
gr <- igraph::graph_from_edgelist(el, directed = FALSE)
gr2 <- igraph::add.edges(gr, edges=c("a", "b"))

set.seed(0)
M <- matrix(rnorm(n=90), ncol=9, dimnames=list(letters[1:10], paste0("s", 1:9)))
M["a", 1:3] <- M["a", 1:3]+5
pheno <- rep(c("trt1", "trt2", "ctrl"), each=3)

gr <- edgelist2graph(el)
kk <- graph2kernel(gr)
gmt <- list(pwy1=list(name="pwy1", description="pwy1", genes=c("a", "b", "c")),
            pwy2=list(name="pwy2", description="pwy2", genes=c("b", "c", "d")))
G <- gmt2Gmat(gmt)

# pants calls match_mets in helper internally

contrast.v <- c(trt1="trt1-ctrl", trt2="trt2-ctrl")
res <- pants(object=M, phenotype.v=pheno, contrast.v=contrast.v[1], ker=kk, Gmat=G, nperm=10)
score.v <- stats::setNames(res$feature.stats$score, nm=rownames(res$feature.stats))