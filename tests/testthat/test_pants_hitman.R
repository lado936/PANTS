context("pants_hitman")

pheno.mat <- ezlimma::batch2design(pheno)
rownames(pheno.mat) <- colnames(M)
nperm <- 100

test_that("kernel", {
  res <- pants_hitman(object=M, exposure = pheno.mat, phenotype.v = M["a",], ker=kk, Gmat=G, nperm=nperm, 
                         ret.null.mats=TRUE)
  pwy.stats <- res$pwy.stats
  f.stats <- res$feature.stats
  nfm <- res$null.feature.mat
  npm <- res$null.pwy.mat

  expect_lte(f.stats["a", "p"], 0.05)
  expect_gte(min(f.stats[-1, "p"]), 0.1)
  
  expect_equal(pwy.stats$nfeatures[1:2], c(3,3))
  
  expect_lt(pwy.stats["pwy1", "p"], pwy.stats["pwy2", "p"])
  
  eval.v <- pwy.stats["pwy1", "feat.score.avg"]*pwy.stats$nfeatures[1]
  expect_equal((sum(eval.v < npm["pwy1",]) + 0.5*sum(eval.v == npm["pwy1",]) + 1)/(nperm+1),
               pwy.stats["pwy1", "p"])
  
  expect_equal(ncol(nfm), nperm)
  expect_equal(ncol(npm), nperm)
  
  #ignore ties, which don't happen here
  expect_lte(mean(f.stats["a", "score"] < nfm["a",]), f.stats["a", "p"]) 
  expect_lte(mean(f.stats["b", "score"] < nfm["b",]), f.stats["b", "p"])
})

test_that("univariate exposure", {
  pheno.v <- setNames(as.numeric(pheno==pheno[1]), nm=names(pheno))
  res <- pants_hitman(object=M, exposure = pheno.v, phenotype.v = M["a",], ker=kk, Gmat=G, nperm=nperm)
  pwy.stats <- res$pwy.stats
  
  expect_lte(pwy.stats["pwy1", "p"], pwy.stats["pwy2", "p"])
  expect_lt(pwy.stats["pwy1", "p"], 0.05)
})

test_that("no kernel", {
  res <- pants_hitman(object=M, exposure = pheno.mat, phenotype.v = M["a",], ker=NULL, Gmat=G, nperm=nperm)
  pwy.stats <- res$pwy.stats
  f.stats <- res$feature.stats
  expect_false("feat.score.avg" %in% colnames(pwy.stats))
  
  expect_lte(f.stats["a", "p"], 0.05)
  expect_gte(min(f.stats[-1, "p"]), 0.1)
  
  expect_equal(pwy.stats$nfeatures[1:2], c(3,3))
  
  expect_lte(pwy.stats["pwy1", "p"], 0.1)
  expect_gte(pwy.stats["pwy2", "p"], 0.1)
})

test_that("min.size", {
  expect_error(pants_hitman(object=M, exposure = pheno.mat, phenotype.v = M["a",], ker=NULL, Gmat=G, nperm=10, min.size=4))
  res3 <- pants_hitman(object=M, exposure = pheno.mat, phenotype.v = M["a",], ker=NULL, Gmat=G[1:3,], nperm=10, min.size=3)
  expect_equal(nrow(res3$pwy.stats), 1)
  res4 <- pants_hitman(object=M, exposure = pheno.mat, phenotype.v = M["a",], ker=NULL, Gmat=G[1:3,], nperm=10, min.size=0)
  expect_equal(nrow(res4$pwy.stats), 2)
})