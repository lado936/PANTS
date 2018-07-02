context("pants_mediation")

test_that("Only 1st feature mediates", {
  nperm <- 100
  res <- pants_mediation(object=M, exposure.v = pheno, phenotype.v = M["a",], ker=kk, Gmat=G, nperm=nperm, 
                         ret.null.mats=TRUE)
  pwy.stats <- res$pwy.stats
  f.stats <- res$feature.stats
  nfm <- res$null.feature.mat
  npm <- res$null.pwy.mat

  expect_lte(f.stats["a", "p"], 0.05)
  expect_gte(min(f.stats[-1, "p"]), 0.1)
  
  expect_lte(pwy.stats["pwy1", "p"], 0.05)
  expect_gte(pwy.stats["pwy2", "p"], 0.1)
  
  expect_equal(ncol(nfm), nperm)
  expect_equal(ncol(npm), nperm)
  
  #ignore ties, which don't happen here
  expect_lte(mean(f.stats["a", "score"] < nfm["a",]), f.stats["a", "p"]) 
  expect_lte(mean(f.stats["b", "score"] < nfm["b",]), f.stats["b", "p"])
})
