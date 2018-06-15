context("pants_mediation")

test_that("Only 1st feature mediates", {
  set.seed(0)
  res <- pants_mediation(object=M, exposure.v = pheno, phenotype.v = M["a",], ker=kk, Gmat=G, nperm=100)
  pwy.stats <- res$pwy.stats
  f.stats <- res$feature.stats
  
  expect_lte(f.stats["a", "p"], 0.05)
  expect_gte(min(f.stats[-1, "p"]), 0.1)
  
  expect_lte(pwy.stats["pwy1", "p"], 0.05)
  expect_gte(pwy.stats["pwy2", "p"], 0.1)
})
