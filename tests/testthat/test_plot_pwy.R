context("plot_pwy")

test_that("returned object", {
  expect_error(plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, annot=NULL, name = NA))
  expect_error(plot_pwy(gr=gr2, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA))
  
  dpn <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA)
  #vertex a is most significant or tied
  expect_gte(dpn$score["a"], max(dpn$score[-1]))
  #based on plot
  expect_gt(dpn$score["a"], 7)
  expect_lt(abs(dpn$score["c"]), 1)
  #pwy colors
  expect_equal(length(unique(dpn$vertex.shape[gmt$pwy1$genes])), 1)
  expect_true(dpn$vertex.color["a"]!=dpn$vertex.color["d"])
  #test equivalence using gr2kernel
  expect_equal(graph2kernel(dpn$gr), graph2kernel(igraph::simplify(gr)))
})

test_that("vdiffr", {
  pp <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA)
  vdiffr::expect_doppelganger(title="pwy1", fig=pp)
})

test_that("alternative", {
  dpn.l <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, alternative = "less", ntop=2)
  vdiffr::expect_doppelganger(title="pwy1_less", fig=dpn.l)
  
  dpn.g <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, alternative = "greater", ntop=1)
  vdiffr::expect_doppelganger(title="pwy1_greater", fig=dpn.g)
})