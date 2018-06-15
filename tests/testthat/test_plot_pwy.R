context("plot_pwy")

test_that("plot_pwy", {
  dpn <- plot_pwy(gr=gr, ker=kk, Gmat=G, pwy="pwy1", score.v=score.v, name = NA)
  #vertex a is most significant or tied
  expect_gte(dpn$score["a"], max(dpn$score[-1]))
  #pwy colors
  expect_equal(length(unique(dpn$vertex.shape[gmt$pwy1$genes])), 1)
  expect_true(dpn$vertex.color["a"]!=dpn$vertex.color["d"])
  #test equivalence using gr2kernel
  expect_equal(graph2kernel(dpn$gr), graph2kernel(igraph::simplify(gr)))
})