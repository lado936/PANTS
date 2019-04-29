context("pants_hitman")

test_that("kernel", {
  res <- pants_hitman(object=M, exposure = pheno.num, phenotype = M["a",], ker=kk, Gmat=G, nperm=nperm, 
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
  res <- pants_hitman(object=M, exposure = pheno.v, phenotype = M["a",], ker=kk, Gmat=G, nperm=nperm)
  pwy.stats <- res$pwy.stats
  
  expect_lte(pwy.stats["pwy1", "p"], pwy.stats["pwy2", "p"])
  expect_lt(pwy.stats["pwy1", "p"], 0.05)
})

test_that("permutations stats not duplicated in parallization", {
  res2 <- pants_hitman(object=M, exposure = pheno.num, phenotype = M["a",], ker=kk, Gmat=G, nperm=nperm, 
                      ret.null.mats=TRUE)
  # independent perms not corrupted by parallelization
  npm <- res2$null.pwy.mat
  nfm <- res2$null.feature.mat
  sp <- res2$sample.perms
  n.unique.perm <- sum(!duplicated(sp, MARGIN=2)) # n unique perms
  expect_equal(sum(!duplicated(npm, MARGIN = 2)), n.unique.perm)
  expect_equal(sum(!duplicated(nfm, MARGIN = 2)), n.unique.perm)
})

test_that("no kernel", {
  res <- pants_hitman(object=M, exposure = pheno.num, phenotype = M["a",], ker=NULL, Gmat=G, nperm=nperm)
  pwy.stats <- res$pwy.stats
  f.stats <- res$feature.stats
  expect_false("feat.score.avg" %in% colnames(pwy.stats))
  
  expect_lte(f.stats["a", "p"], 0.05)
  expect_gte(min(f.stats[-1, "p"]), 0.1)
  
  expect_equal(pwy.stats$nfeatures[1:2], c(3,3))
  
  expect_lte(pwy.stats["pwy1", "p"], 0.1)
  expect_gte(pwy.stats["pwy2", "p"], 0.1)
})

test_that("min.nfeats", {
  expect_error(pants_hitman(object=M, exposure = pheno.num, phenotype = M["a",], ker=NULL, Gmat=G, nperm=10, min.nfeats=4))
  res3 <- pants_hitman(object=M, exposure = pheno.num, phenotype = M["a",], ker=NULL, Gmat=G[1:3,], nperm=10, min.nfeats=3)
  expect_equal(nrow(res3$pwy.stats), 1)
  res4 <- pants_hitman(object=M, exposure = pheno.num, phenotype = M["a",], ker=NULL, Gmat=G[1:3,], nperm=10, min.nfeats=0)
  expect_equal(nrow(res4$pwy.stats), 2)
})

test_that("write with feat.tab & impact", {
  res <- pants_hitman(object=M, exposure = pheno.num, phenotype = M["a",], ker=kk, Gmat=G, feat.tab = eztt.df, nperm=50, ntop=5,
               name="test_eztt")
  ps.xl <- ezlimma::read_linked_xl("test_eztt_pants_hitman/test_eztt_pants_hitman.xlsx")
  pwy1 <- read.csv("test_eztt_pants_hitman/pathways/pwy1.csv", row.names = 1, stringsAsFactors = FALSE)
  
  expect_false("feat.score.avg" %in% colnames(ps.xl))
  expect_equal(nrow(pwy1), 4)
  expect_equal(pwy1$sym[1], "a")
  
  # impact = Ki*Gj*zi
  zscore.v <- stats::setNames(res$feature.stats$z, nm=rownames(res$feature.stats))
  impact.v <- (kk %*% G[,"pwy1"])[,1] * zscore.v
  expect_equal(signif(impact.v[rownames(pwy1)], 3), setNames(pwy1$impact, nm=rownames(pwy1)))
})

teardown({
  unlink("test_eztt_pants_hitman", recursive = TRUE, force=TRUE)
  unlink("tests/testthat/test_eztt_pants_hitman", recursive = TRUE, force=TRUE)
})