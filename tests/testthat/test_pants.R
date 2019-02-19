context("pants")

ff <- function(v) v[2]-v[1]

test_that("helper_pants", {
  #res is from helper_pants.R
  expect_equal(res$pwy.stats$nfeatures, c(3,3))
  #allow for =, since only 10 perm
  expect_gte(res$pwy.stats["pwy1", 2], res$pwy.stats["pwy2", 2])
  expect_equal(res$pwy.stats["pwy1", 1], 3)
  expect_equal(res$pwy.stats["pwy2", 1], 3)
  expect_gt(res$feature.stats["a", 1], max(res$feature.stats[setdiff(rownames(kk), "a"), 1]))
})

test_that("kernel & parallel", {
  #contr of length 2
  expect_error(pants(object=M, phenotypes.v=pheno, contrast.v=contrast.v, ker=kk, Gmat=G, nperm=10))
  ff <- function(v) v[2]-v[1]
  res2 <- pants(object=M, phenotype=pheno, contrast.v=contrast.v, ker=kk, Gmat=G, nperm=10, score_fcn = ff, 
                ret.null.mats = TRUE, ncores=2)
  expect_equal(nrow(res2$pwy.stats), 2)
  
  expect_equal(res2$pwy.stats$nfeatures, c(3,3))
  npm <- res2$null.pwy.mat
  expect_gte(length(unique(npm[1,])), 7) #independent perms
  pwy1.p <- p_ecdf(eval.v=3*res2$pwy.stats["pwy1", 2], score.mat=t(as.matrix(npm["pwy1",])))
  expect_equal(pwy1.p[1, "p"], setNames(res2$pwy.stats["pwy1", "p"], nm="p"))
})

test_that("no kernel", {
  res2 <- pants(object=M, phenotype=pheno, contrast.v=contrast.v, ker=NULL, Gmat=G, nperm=10, score_fcn = ff)
  expect_equal(res2$pwy.stats$nfeatures, c(3,3))
})

test_that("min.nfeats", {
  expect_error(pants(object=M, phenotype=pheno, contrast.v=contrast.v, ker=NULL, Gmat=G, nperm=10, score_fcn = ff, min.nfeats=4))
  res3 <- pants(object=M, phenotype=pheno, contrast.v=contrast.v, ker=NULL, Gmat=G[1:3,], nperm=10, score_fcn = ff, min.nfeats=3)
  expect_equal(nrow(res3$pwy.stats), 1)
  res4 <- pants(object=M, phenotype=pheno, contrast.v=contrast.v, ker=NULL, Gmat=G[1:3,], nperm=10, score_fcn = ff, min.nfeats=0)
  expect_equal(nrow(res4$pwy.stats), 2)
})

test_that("write with feat.tab", {
  res <- pants(object=M, phenotype=pheno, contrast.v=contrast.v[1], ker=kk, Gmat=G, feat.tab = eztt.df, nperm=10, ntop=5,
               name="test_eztt")
  pwy1 <- read.csv("test_eztt_pants/pathways/pwy1.csv", row.names = 1, stringsAsFactors = FALSE)

  expect_equal(nrow(pwy1), 4)
  expect_equal(pwy1$sym, c("a", "b", "d", "c"))
  expect_lt(pwy1["a", "trt1.p"], res$feature.stats["a", "p"])
  
  res <- pants(object=M, phenotype=pheno, contrast.v=contrast.v[1], ker=noker, Gmat=G, feat.tab = eztt.df, nperm=10, ntop=2,
               name="test_eztt", alternative = "greater")
  pwy1 <- read.csv("test_eztt_pants/pathways/pwy1.csv", row.names = 1, stringsAsFactors = FALSE)
  expect_equal(nrow(pwy1), 2)
  expect_equal(pwy1$sym, c("a", "b"))
})

teardown({
  unlink("test_eztt_pants", recursive = TRUE, force=TRUE)
  unlink("tests/testthat/test_eztt_pants", recursive = TRUE, force=TRUE)
})