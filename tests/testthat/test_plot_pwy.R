context("plot_pwy")

dpn <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA)

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
  
  dpn2 <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, alternative = "less")
  expect_equal(dpn$score, dpn2$score)
  
  ppl <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, alternative = "less", ntop=2)
  expect_equal(signif(ppl$score, 2), c(b=-1.1, c=0.43))
  
  #fig changes if seed changes
  ppl.f <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, alternative = "less", 
                               ntop=2, seed=1)
  expect_doppelganger(title="pwy1-less", ppl.f)
  
  ppg <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, alternative = "greater", ntop=2)
  expect_equal(ppg$score, score.v[c(1:2, 4, 3)])
})

test_that("annot", {
  annot <- setNames(LETTERS[1:3], nm=names(score.v)[1:3])
  dpn.ann <- function() plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, annot = annot)
  expect_doppelganger("pwy1-ann", dpn.ann)
  
  expect_error(plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA, annot = c(A="A")))
})