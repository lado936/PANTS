context("pants-mediation")

test_that("kernel", {
  res <- pants(type="mediation", object=M, exposure = pheno.num, phenotype = M["a",], ker=kk, Gmat=G, nperm=nperm, 
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
  res <- pants(type="mediation", object=M, exposure = pheno.v, phenotype = M["a",], ker=kk, Gmat=G, nperm=nperm)
  pwy.stats <- res$pwy.stats
  
  expect_lte(pwy.stats["pwy1", "p"], pwy.stats["pwy2", "p"])
  expect_lt(pwy.stats["pwy1", "p"], 0.05)
})

test_that("permutations stats not duplicated in parallization", {
  res2 <- pants(type="mediation", object=M, exposure = pheno.num, phenotype = M["a",], ker=kk, Gmat=G, nperm=nperm, 
                      ret.null.mats=TRUE)
  # independent perms not corrupted by parallelization
  npm <- res2$null.pwy.mat
  nfm <- res2$null.feature.mat
  sp <- res2$sample.perms
  n.unique.perm <- sum(!duplicated(sp, MARGIN=2)) # n unique perms
  expect_equal(sum(!duplicated(npm, MARGIN = 2)), n.unique.perm)
  expect_equal(sum(!duplicated(nfm, MARGIN = 2)), n.unique.perm)
})

test_that("no kernel & ret null mats", {
  res <- pants(type="mediation", object=M, exposure = pheno.num, phenotype = M["a",], ker=NULL, Gmat=G, nperm=nperm, ret.null.mats = TRUE)
  pwy.stats <- res$pwy.stats
  f.stats <- res$feature.stats
  expect_false("feat.score.avg" %in% colnames(pwy.stats))
  
  expect_lte(f.stats["a", "p"], 0.05)
  expect_gte(min(f.stats[-1, "p"]), 0.1)
  
  expect_equal(pwy.stats$nfeatures[1:2], c(3,3))
  
  expect_lte(pwy.stats["pwy1", "p"], 0.1)
  expect_gte(pwy.stats["pwy2", "p"], 0.1)
  
  # ret null
  expect_equal(p_ecdf(res$pwy.stats$score, res$null.pwy.mat, alternative = "greater"),
               data.matrix(res$pwy.stats[, c("z", "p")]))
})

test_that("min.nfeats", {
  expect_error(pants(type="mediation", object=M, exposure = pheno.num, phenotype = M["a",], ker=NULL, Gmat=G, nperm=10, min.nfeats=4))
  res3 <- pants(type="mediation", object=M, exposure = pheno.num, phenotype = M["a",], ker=NULL, Gmat=G[1:3,], nperm=10, min.nfeats=3)
  expect_equal(nrow(res3$pwy.stats), 1)
  res4 <- pants(type="mediation", object=M, exposure = pheno.num, phenotype = M["a",], ker=NULL, Gmat=G[1:3,], nperm=10, min.nfeats=0)
  expect_equal(nrow(res4$pwy.stats), 2)
})

test_that("write with annot.df & impact", {
  res <- pants(type="mediation", object=M, exposure = pheno.num, phenotype = M["a",], ker=kk, Gmat=G, annot.df = eztt.df, nperm=50, ntop=5,
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

test_that("size & power", {
  set.seed(1)
  ngenes <- 100
  gene.nms <- paste0("g", 1:ngenes)
  gmt <- apply(as.matrix(1:ngenes), 1, FUN=function(x){
    pwy.nm <- paste0("pwy", x)
    ret <- list(name=pwy.nm, description=pwy.nm, genes=sample(gene.nms, size=5))
  })
  G <- gmt2Gmat(gmt)
  el <- t(combn(rownames(G), 2))
  el <- el[-sample(nrow(el), size=floor(nrow(el)/2)),]
  gr <- edgelist2graph(el)
  ker <- graph2kernel(gr)
  sp <- sim_pants_mediation(Gmat=G, exposure = pheno.num, nsim=5, nperm=25, effect.v = c(0, 0.5), ker=ker)
  expect_lte(sp[1, 1], 0.06)
  expect_gte(sp[1, 2], 0.3)
})

teardown({
  tep.dir <- test_path("test_eztt_pants_hitman")
  unlink(tep.dir, recursive = TRUE, force=TRUE)
})