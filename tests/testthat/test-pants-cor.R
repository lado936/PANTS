context("pants cor")

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
  pheno.norm <- setNames(rnorm(length(pheno)), paste0("s", 1:length(pheno)))
  sp <- sim_pants(Gmat=G, phenotype = pheno.norm, nsim=10, nperm=50, effect.v = c(0, 0.2), ker=ker, type = "correlation")
  expect_lte(sp[1, 1], 0.1)
  expect_gte(sp[1, 2], 0.4)
})