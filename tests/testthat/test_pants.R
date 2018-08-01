context("pants")

test_that("pants", {
  #contr of length 2
  expect_error(pants(object=M, phenotypes.v=pheno, contrast.v=contrast.v, ker=kk, Gmat=G, nperm=10))
  ff <- function(v) v[2]-v[1]
  res2 <- pants(object=M, phenotype.v=pheno, contrast.v=contrast.v, ker=kk, Gmat=G, nperm=10, score_fcn = ff, 
                ret.null.mats = TRUE)
  expect_equal(nrow(res2$pwy.stats), 2)
  
  expect_gt(res$pwy.stats["pwy1", 2], res$pwy.stats["pwy2", 2])
  expect_equal(res$pwy.stats["pwy1", 1], 3)
  expect_equal(res$pwy.stats["pwy2", 1], 3)
  expect_gt(res$feature.stats["a", 1], max(res$feature.stats[setdiff(rownames(kk), "a"), 1]))
  
  npm <- res2$null.pwy.mat
  pwy1.p <- p_ecdf(eval.v=3*res$pwy.stats["pwy1", 2], score.mat=t(as.matrix(npm["pwy1",])))
  expect_equal(pwy1.p[1, "p"], setNames(res$pwy.stats["pwy1", "p"], nm="p"))
})
