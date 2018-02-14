library("PANTS")
library("ezlimma")
context("utils")

#sq w/ all nodes connected except a <-> c; a <-> d repeated, as in sif
el <- rbind(t(combn(letters[1:4], 2))[-2,], c("a", "d"))
gr <- graph_from_edgelist(el, directed = FALSE)

set.seed(0)
M <- matrix(rnorm(n=60), ncol=6, dimnames=list(letters[1:10], paste0("s", 1:6)))
M["a", 1:3] <- M["a", 1:3]+5
pheno <- rep(c("trt", "ctrl"), each=3)

kk <- edgeList2kernel(el)
gmt <- list(pwy1=list(name="pwy1", description="pwy1", genes=c("a", "b", "c")),
            pwy2=list(name="pwy2", description="pwy2", genes=c("b", "c", "d")))
G <- gmt2Gmat(gmt)
G2 <- matchGmat2Ker(G, kk)

res <- pants(object=M, phenotypes.v=pheno, contrasts.v="trt-ctrl", ker=kk, Gmat=G2, nperm=10, ret.null.mats = T)
pv <- setNames(res$feature.stats$pval, nm=rownames(res$feature.stats))

##tests
test_that("mat_pow on dense matrix", {
  #from https://www.mathworks.com/help/matlab/ref/mpower.html
  x <- matrix(1:4, nrow=2, byrow = TRUE)
  expect_equal(mat_pow(x, 2), matrix(c(7,10,15,22), nrow=2, byrow = TRUE))
})

test_that("mat_pow on sparse matrix", {
  x <- Matrix(1:4, nrow=2, byrow = TRUE)
  expect_equal(mat_pow(x, 2), Matrix(c(7,10,15,22), nrow=2, byrow = TRUE))
})

test_that("edgeList2kernel", {
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
  sf <- score_features(object=M, phenotypes.v=pheno, contrasts.v="trt-ctrl", score_fcn=abs)
  expect_equal(which.max(sf), 1)
})

test_that("gmt2Gmat", {
  expect_true(all(G@x == 1))
  expect_equal(sum(G), 6)
  expect_equal(G[4,1], 0)
  expect_equal(G[1,2], 0)
})

test_that("matchGmat2Ker", {
  expect_equal(rownames(G2), colnames(kk))
  expect_equal(sum(G2), 6)
})

test_that("pants", {
  expect_gt(res$pwy.stats["pwy1", 1], res$pwy.stats["pwy2", 1])
  expect_equal(res$pwy.stats["pwy1", 2], 3)
  expect_equal(res$pwy.stats["pwy2", 2], 3)
  expect_gt(res$feature.stats["a", 1], max(res$feature.stats[setdiff(rownames(kk), "a"), 1]))
})

test_that("draw_pwy_net", {
  expect_warning(dpn <- draw_pwy_net(gr=gr, ker=kk, Gmat=G2, pwy="pwy1", pval.v=pv, name.pdf = NA))
  #vertex a is most significant or tied
  expect_gte(dpn$vertex.size["a"], max(dpn$vertex.size[-1]))
  #pwy colors
  expect_equal(length(unique(dpn$vertex.color[gmt$pwy1$genes])), 1)
  expect_true(dpn$vertex.color["a"]!=dpn$vertex.color["d"])
  #test equivalence using gr2kernel
  expect_equal(gr2kernel(dpn$gr), gr2kernel(igraph::simplify(gr)))
})