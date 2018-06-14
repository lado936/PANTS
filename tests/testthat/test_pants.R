library("ezlimma")
library("PANTS")
library("testthat")
context("pants")

#sq w/ all nodes connected except a <-> c; a <-> d repeated, as in sif
el <- rbind(t(combn(letters[1:4], 2))[-2,], c("a", "d"))
gr <- igraph::graph_from_edgelist(el, directed = FALSE)

set.seed(0)
M <- matrix(rnorm(n=90), ncol=9, dimnames=list(letters[1:10], paste0("s", 1:9)))
M["a", 1:3] <- M["a", 1:3]+5
pheno <- rep(c("trt1", "trt2", "ctrl"), each=3)

gr <- edgelist2graph(el)
kk <- graph2kernel(gr)
gmt <- list(pwy1=list(name="pwy1", description="pwy1", genes=c("a", "b", "c")),
            pwy2=list(name="pwy2", description="pwy2", genes=c("b", "c", "d")))
G <- gmt2Gmat(gmt)

# mm <- match_mats(score.mat=M, ker=kk, Gmat=G)
# M <- mm$score.mat; kk=mm$ker; G <- mm$Gmat

contrast.v <- c(trt1="trt1-ctrl", trt2="trt2-ctrl")
res <- pants(object=M, phenotype.v=pheno, contrast.v=contrast.v[1], ker=kk, Gmat=G, nperm=10)
score.v <- stats::setNames(res$feature.stats$score, nm=rownames(res$feature.stats))

##tests
test_that("clean pwycommons SIF", {
  sif <- cbind(c(letters[c(2,1,2,2,3)], ""), "interacts-with", letters[c(1,3,1,3,1,1)])
  el.o <- t(apply(sif[,c(1,3)], 1, FUN=sort))
  el2 <- sif2edgelist(sif)
  expect_equal(el2, el.o[c(1,2,4,6),])
  el3 <- sif2edgelist(sif[c(1,2,4,6),])
  expect_equal(el3, el.o[c(1,2,4,6),])
  
  el4 <- sif2edgelist(sif, rm.ids = c("", "c", "water"))
  expect_equal(el4, el2[1,,drop=FALSE])
})

test_that("mat_pow on dense matrix", {
  #from https://www.mathworks.com/help/matlab/ref/mpower.html
  x <- matrix(1:4, nrow=2, byrow = TRUE)
  expect_equal(mat_pow(x, 2), matrix(c(7,10,15,22), nrow=2, byrow = TRUE))
})

test_that("mat_pow on sparse matrix", {
  x <- Matrix(1:4, nrow=2, byrow = TRUE)
  expect_equal(mat_pow(x, 2), Matrix(c(7,10,15,22), nrow=2, byrow = TRUE))
})

test_that('mat_pow > 2', {
  mat <- matrix(1:9,nrow = 3)
  mat4pow <- mat %*% mat
  mat4pow <- mat4pow %*% mat
  mat4pow <- mat4pow %*% mat
  expect_equal(mat_pow(mat,4),mat4pow)
})

test_that("edgelist -> kernel", {
  expect_equal(kk[2,2], 1)
  expect_true(isSymmetric(as.matrix(kk)))
  #B & D are equivalent
  expect_equal(kk["a", "b"], kk["a", "d"])
  #a <-> c not connected, so k=0
  expect_equal(kk["a", "c"], 0)
  #a=2, p=1 yields similarities
  expect_true(all(kk@x <= 1))
  expect_true(all(kk@x >= 0))
})

test_that("score_features", {
  sf <- score_features(object=M, phenotype.v=pheno, contrast.v="trt1-ctrl", score_fcn=abs)
  expect_equal(which.max(sf), c(a=1))
})

test_that("gmt2Gmat", {
  expect_true(all(G@x == 1))
  expect_equal(sum(G), 6)
  expect_equal(G[4,1], 0)
  expect_equal(G[1,2], 0)
})

test_that("match_mats", {
  expect_gt(length(intersect(rownames(G), colnames(kk))), 0)
  expect_equal(sum(G), 6)
  
  M2 <- M[,pheno != 'trt2']
  pheno2 <- pheno[pheno != 'trt2']
  score_fcn <- identity
  contrast.v2 <- contrast.v[1]
  nperm <- 10
  score.v <- score_features(object= M2, phenotype.v= pheno2, contrast.v= contrast.v2, score_fcn= score_fcn )
  
  #feature scores in permutations, 74% dense but later combine with a sparse (empty) matrix
  score.mat <- Matrix::Matrix(0, nrow=nrow(M2), ncol=nperm, dimnames = list(rownames(M2), paste0('perm', 1:nperm)))
  for (perm in 1:nperm){
    #must set permuted names to NULL st limma_contrasts doesn't complain thay they clash with colnames(object)
    pheno.tmp <- stats::setNames(pheno2[sample(1:length(pheno2))], nm=NULL)
    
    score.mat[,perm] <- score_features(object=M2, phenotype.v=pheno.tmp, contrast.v=contrast.v2, score_fcn=score_fcn)

  }
  
  mm <- match_mats(score.mat = cbind(v=score.v, score.mat), ker=kk, Gmat=G)
  
  expect_equal(dimnames(mm$score.mat)[1],dimnames(mm$ker)[1])
  expect_equal(dimnames(mm$score.mat)[1],dimnames(mm$Gmat)[1])
  
})

test_that("pants", {
  #contr of length 2
  expect_error(pants(object=M, phenotypes.v=pheno, contrast.v=contrast.v, ker=kk, Gmat=G, nperm=10))
  ff <- function(v) v[2]-v[1]
  res2 <- pants(object=M, phenotype.v=pheno, contrast.v=contrast.v, ker=kk, Gmat=G, nperm=10, score_fcn = ff)
  expect_equal(nrow(res2$pwy.stats), 2)
  
  expect_gt(res$pwy.stats["pwy1", 2], res$pwy.stats["pwy2", 2])
  expect_equal(res$pwy.stats["pwy1", 1], 3)
  expect_equal(res$pwy.stats["pwy2", 1], 3)
  expect_gt(res$feature.stats["a", 1], max(res$feature.stats[setdiff(rownames(kk), "a"), 1]))
})

test_that("plot_pwy", {
  dpn <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA)
  #vertex a is most significant or tied
  expect_gte(dpn$score["a"], max(dpn$score[-1]))
  #pwy colors
  expect_equal(length(unique(dpn$vertex.shape[gmt$pwy1$genes])), 1)
  expect_true(dpn$vertex.color["a"]!=dpn$vertex.color["d"])
  #test equivalence using gr2kernel
  expect_equal(graph2kernel(dpn$gr), graph2kernel(igraph::simplify(gr)))
})

