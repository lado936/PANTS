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

gmt <- list(pwy1=list(name="pwy1", description="pwy1", genes=c("a", "b", "c")),
            pwy2=list(name="pwy2", description="pwy2", genes=c("b", "c", "d")))
G <- gmt2Gmat(gmt)

gr <- edgelist2graph(el)
kk0 <- graph2kernel(gr)
kk <- kk0[rownames(G), rownames(G)]

noker <- diag_kernel(object=M, Gmat=G)

contrast.v <- c(trt1="trt1-ctrl", trt2="trt2-ctrl")
res <- pants(object=M, phenotype=pheno, contrast.v=contrast.v[1], ker=kk, Gmat=G, nperm=50)
score.v <- stats::setNames(res$feature.stats$score, nm=rownames(res$feature.stats))
# feature z-score from permutations
zscore.v <- stats::setNames(res$feature.stats$z, nm=rownames(res$feature.stats))

res.noker <- pants(object=M, phenotype=pheno, contrast.v=contrast.v[1], Gmat=G, nperm=10)
score.noker <- stats::setNames(res.noker$feature.stats$score, nm=rownames(res.noker$feature.stats))

fl <- lapply(gmt, FUN=function(x) x$genes)
names(fl) <- lapply(gmt, FUN=function(x) x$name)

eztt <- ezlimma::limma_contrasts(M, grp=pheno, contrast.v = contrast.v)
eztt.df <- data.frame(signif(eztt, 3), sym=rownames(eztt))

pheno.num <- as.numeric(pheno == "trt1")
names(pheno.num) <- colnames(M)
nperm <- 100

ff <- function(v) v[2]-v[1]

# d? plot network
dpn <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v, name = NA, plot=FALSE, annot.v = NA, 
                alternative = "two.sided", signif.dig=2, seed = 0, ntop=7)