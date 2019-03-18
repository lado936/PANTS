context("plot_pwy")

#d? plot network
dpn <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, plot=FALSE)

test_that("returned object", {
  expect_error(plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, annot=NULL, name = NA))
  expect_error(plot_pwy(gr=gr2, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA))
  
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

test_that("alternative", {
  dpn.f <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA)
  expect_doppelganger(title="pwy1", dpn.f)
  
  dpn2 <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, alternative = "less", plot=FALSE)
  expect_equal(dpn$score, dpn2$score)
  
  ppl <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, alternative = "less", ntop=2, plot=FALSE)
  expect_equal(ppl$top.node.nms, c("b", "a"))
  
  #fig changes if seed changes
  ppl.f <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, alternative = "less", 
                               ntop=2, seed=1)
  expect_doppelganger(title="pwy1-less", ppl.f)
  
  ppg <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, alternative = "greater", ntop=2, plot=FALSE)
  expect_equal(ppg$score, score.v[c(1:2)])
})

test_that("annot", {
  annot <- setNames(LETTERS[1:3], nm=names(score.v)[1:3])
  dpn.ann <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, annot = annot)
  expect_doppelganger("pwy1-ann", dpn.ann)
  
  expect_error(plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, annot = c(A="A")))
})

test_that("negatives with decimals", {
  dpn.f <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v/10, name = NA)
  expect_doppelganger(title="pwy1-neg", dpn.f)
})

test_that("no kernel", {
  dpn.noker <- function() plot_pwy(gr=gr, Gmat=G, pwy="pwy2", score.v=score.v, name = NA, ntop = 3)
  #should not have a
  expect_doppelganger(title="pwy2-noker", dpn.noker)
})

test_that("analyte in G & kernel but not score.v", {
  dpn.f <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy2", score.v=score.v[-4], name = NA, ntop=4)
  expect_doppelganger(title="pwy2-na", dpn.f)
})