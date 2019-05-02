context("pants")

test_that("helper_pants", {
  # from helper_pants.R
  expect_equal(pwy.stats$nfeatures, c(3,3))
  #allow for =, since only 10 perm
  expect_gte(pwy.stats["pwy1", 2], pwy.stats["pwy2", 2])
  expect_equal(pwy.stats["pwy1", 1], 3)
  expect_equal(pwy.stats["pwy2", 1], 3)
  expect_gt(feature.stats["a", 1], max(feature.stats[setdiff(rownames(kk), "a"), 1]))
})

test_that("kernel & parallel", {
  #contr of length 2
  expect_error(pants(object=M, phenotypes.v=pheno, contrast.v=contrast.v, ker=kk, Gmat=G, nperm=10))
  res2 <- pants(object=M, phenotype=pheno, contrast.v=contrast.v, ker=kk, Gmat=G, nperm=10, score_fcn = ff, 
                ret.null.mats = TRUE, ncores=2)
  expect_equal(nrow(res2$pwy.stats), 2)
  expect_equal(res2$pwy.stats$nfeatures, c(3,3))
  # independent perms not corrupted by parallelization
  npm <- res2$null.pwy.mat
  nfm <- res2$null.feature.mat
  sp <- res2$sample.perms
  n.unique.perm <- sum(!duplicated(sp, MARGIN=2)) # n unique perms
  expect_equal(length(unique(npm[1,])), n.unique.perm)
  expect_equal(length(unique(npm[2,])), n.unique.perm)
  expect_equal(length(unique(nfm[1,])), n.unique.perm)
  expect_equal(length(unique(nfm[2,])), n.unique.perm)
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

test_that("write with feat.tab & test impact", {
  res <- pants(object=M, phenotype=pheno, contrast.v=contrast.v[1], ker=kk, Gmat=G, feat.tab = eztt.df, nperm=10, ntop=5,
               name="test_eztt")
  pwy1 <- read.csv("test_eztt_pants/pathways/pwy1.csv", row.names = 1, stringsAsFactors = FALSE)
  expect_equal(nrow(pwy1), 4)
  expect_lt(pwy1["a", "trt1.p"], res$feature.stats["a", "p"])
  
  res <- pants(object=M, phenotype=pheno, contrast.v=contrast.v[1], ker=noker, Gmat=G, feat.tab = eztt.df, nperm=10, ntop=2,
               name="test_eztt")
  pwy1 <- read.csv("test_eztt_pants/pathways/pwy1.csv", row.names = 1, stringsAsFactors = FALSE)
  expect_equal(nrow(pwy1), 2)
  
  # impact = Ki*Gj*zi
  zscore.v <- stats::setNames(res$feature.stats$z, nm=rownames(res$feature.stats))
  impact.v <- (noker %*% G[,"pwy1"])[,1] * zscore.v
  expect_equal(signif(impact.v[rownames(pwy1)], 3), setNames(pwy1$impact, nm=rownames(pwy1)))
})

teardown({
  unlink("test_eztt_pants", recursive = TRUE, force=TRUE)
  unlink("tests/testthat/test_eztt_pants", recursive = TRUE, force=TRUE)
})