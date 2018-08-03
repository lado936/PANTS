context("pants")

ff <- function(v) v[2]-v[1]

test_that("kernel", {
  #contr of length 2
  expect_error(pants(object=M, phenotypes.v=pheno, contrast.v=contrast.v, ker=kk, Gmat=G, nperm=10))
  ff <- function(v) v[2]-v[1]
  res2 <- pants(object=M, phenotype.v=pheno, contrast.v=contrast.v, ker=kk, Gmat=G, nperm=10, score_fcn = ff, 
                ret.null.mats = TRUE)
  expect_equal(nrow(res2$pwy.stats), 2)
  
  #res is from helper_pants.R
  expect_equal(res$pwy.stats$nfeatures, c(3,3))
  expect_gt(res$pwy.stats["pwy1", 2], res$pwy.stats["pwy2", 2])
  expect_equal(res$pwy.stats["pwy1", 1], 3)
  expect_equal(res$pwy.stats["pwy2", 1], 3)
  expect_gt(res$feature.stats["a", 1], max(res$feature.stats[setdiff(rownames(kk), "a"), 1]))
  
  expect_equal(res2$pwy.stats$nfeatures, c(3,3))
  npm <- res2$null.pwy.mat
  pwy1.p <- p_ecdf(eval.v=3*res$pwy.stats["pwy1", 2], score.mat=t(as.matrix(npm["pwy1",])))
  expect_equal(pwy1.p[1, "p"], setNames(res$pwy.stats["pwy1", "p"], nm="p"))
})

test_that("no kernel", {
  res2 <- pants(object=M, phenotype.v=pheno, contrast.v=contrast.v, ker=NULL, Gmat=G, nperm=10, score_fcn = ff)
  expect_equal(res2$pwy.stats$nfeatures, c(3,3))
})

test_that("min.size", {
  expect_error(pants(object=M, phenotype.v=pheno, contrast.v=contrast.v, ker=NULL, Gmat=G, nperm=10, score_fcn = ff, min.size=4))
  res3 <- pants(object=M, phenotype.v=pheno, contrast.v=contrast.v, ker=NULL, Gmat=G[1:3,], nperm=10, score_fcn = ff, min.size=3)
  expect_equal(nrow(res3$pwy.stats), 1)
  res4 <- pants(object=M, phenotype.v=pheno, contrast.v=contrast.v, ker=NULL, Gmat=G[1:3,], nperm=10, score_fcn = ff, min.size=0)
  expect_equal(nrow(res4$pwy.stats), 2)
})