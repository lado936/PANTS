context("plot_pwy")

test_that("returned object", {
  expect_error(plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v, annot=NULL, name = NA))
  expect_error(plot_pwy(gr=gr2, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v, name = NA))
  # vertex a is most significant or tied
  expect_gte(dpn$vertex.zscore["a"], max(dpn$vertex.zscore[-1]))
  expect_equal(zscore.v[names(dpn$vertex.zscore)], dpn$vertex.zscore)
  # impact = Ki*Gj*zi
  impact.v <- (kk %*% G[,"pwy1"])[,1] * zscore.v
  expect_equal(impact.v[names(dpn$vertex.impact)], dpn$vertex.impact)
  #pwy colors
  expect_equal(length(unique(dpn$vertex.shape[gmt$pwy1$genes])), 1)
  expect_true(dpn$vertex.color["a"]!=dpn$vertex.color["d"])
  #test equivalence using gr2kernel
  expect_equal(graph2kernel(dpn$gr), graph2kernel(igraph::simplify(gr)))
})

test_that("alternative", {
  dpn.f <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v, name = NA)
  expect_doppelganger(title="pwy1", dpn.f)
  
  dpn2 <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v, name = NA, alternative = "less", plot=FALSE)
  expect_equal(dpn$vertex.zscore, dpn2$vertex.zscore[names(dpn$vertex.zscore)])
  
  ppl <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v, name = NA, alternative = "less", ntop=2, plot=FALSE)
  impact.v <- (kk %*% G[,"pwy1"])[,1] * zscore.v
  impact.less <- impact.v[order(-abs(impact.v))][1:2]
  impact.less <- impact.less[order(impact.less)]
  expect_equal(ppl$vertex.impact, impact.less)
  
  #fig changes if seed changes
  ppl.f <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v, name = NA, alternative = "less", 
                               ntop=2, seed=0)
  expect_doppelganger(title="pwy1-less", ppl.f)
  
  ppg <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v, name = NA, alternative = "greater", ntop=2, plot=FALSE)
  expect_equal(ppg$vertex.zscore, zscore.v[names(ppg$vertex.zscore)])
})

test_that("annot", {
  annot <- setNames(LETTERS[1:3], nm=names(zscore.v)[1:3])
  dpn.ann <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v, name = NA, annot = annot)
  expect_doppelganger("pwy1-ann", dpn.ann)
  expect_error(plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v, name = NA, annot = c(A="A")))
})

test_that("negatives with decimals", {
  dpn.f <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", zscore.v=zscore.v/10, name = NA)
  expect_doppelganger(title="pwy1-neg", dpn.f)
})

test_that("no kernel", {
  dpn.noker <- function() plot_pwy(gr=gr, Gmat=G, pwy="pwy2", zscore.v=zscore.v, name = NA, ntop = 3)
  #should not have a
  expect_doppelganger(title="pwy2-noker", dpn.noker)
})

test_that("analyte in G & kernel but not zscore.v", {
  pp.na <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy2", zscore.v=zscore.v[-4], name = NA, plot = FALSE)
  expect_true(is.na(pp.na$vertex.color["d"]))
})

# this deletes Rplots.pdf 
teardown({
  rplots.dir <- test_path("Rplots.pdf")
  unlink(rplots.dir, force=TRUE)
})