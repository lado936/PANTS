library(testthat)
library(vdiffr)

# many objects used in other tests

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

noker <- diag_kernel(object.rownames=rownames(M), Gmat.rownames=rownames(G))

contrast.v <- c(trt1="trt1-ctrl", trt2="trt2-ctrl")
zeallot::`%<-%`(c(pwy.stats, feature.stats, csv.lst),
                pants(object=M, phenotype=pheno, contrast.v=contrast.v[1], ker=kk, Gmat=G, nperm=50, ret.pwy.dfs = TRUE))
feat.tab <- data.frame(z=feature.stats[, "z", drop=FALSE], annot=NA)
zscore.v <- stats::setNames(feature.stats[, "z"], nm=rownames(feature.stats))
score.v <- stats::setNames(feature.stats[, "score"], nm=rownames(feature.stats))

res.noker <- pants(object=M, phenotype=pheno, contrast.v=contrast.v[1], Gmat=G, nperm=10)
score.noker <- stats::setNames(res.noker$feature.stats$score, nm=rownames(res.noker$feature.stats))

fl <- lapply(gmt, FUN=function(x) x$genes)
names(fl) <- lapply(gmt, FUN=function(x) x$name)

eztt <- ezlimma::limma_contrasts(M, grp=pheno, contrast.v = contrast.v)
eztt.df <- data.frame(signif(eztt, 3), sym=rownames(eztt))

pheno.num <- as.numeric(pheno == "trt1")
names(pheno.num) <- colnames(M)
nperm <- 100

ff <- function(v) abs(v[2]-v[1])
