context("plot_pwy")

test_that("returned object", {
  # tbl_graph is subclass of `igraph`
  # vertex a is most significant or tied
  expect_gte(V(pp)$z[match("a", V(pp)$name)], max(V(pp)$z[-match("a", V(pp)$name)]))
  expect_equal(as.numeric(zscore.v[match(V(pp)$name, names(zscore.v))]), V(pp)$z)
  #test equivalence using gr2kernel
  expect_equal(graph2kernel(pp), graph2kernel(igraph::simplify(gr)))
})

test_that("ntop & seed", {
  pp2 <- function() plot_pwy(feat.tab = feat.tab, impact.tab = csv.lst[[1]], gr=gr, Gmat.pwy = G[,"pwy1",drop=F], name = NA,
                        ntop = 2, plot = TRUE, seed = 0)
  expect_doppelganger(title="pwy1-ntop2", pp2)
  
  pp.s1 <- function() plot_pwy(feat.tab = feat.tab, impact.tab = csv.lst[[1]], gr=gr, Gmat.pwy = G[,"pwy1",drop=F], name = NA,
                             ntop = 2, plot = TRUE, seed = 1)
  expect_doppelganger(title="pwy1-seed1", pp.s1)
})

test_that("annot", {
  csv.nms <- rownames(csv.lst[[1]])
  rownames(feat.tab)[match(csv.nms, rownames(feat.tab))] <- V(gr)$name <- 
    rownames(G)[match(csv.nms, rownames(G))] <- rownames(csv.lst[[1]]) <- 
    c("abijabee3.4", "abijabee3-_4", "oh-not-so-Much", "d")
  pp.ann <- function() plot_pwy(feat.tab = feat.tab, impact.tab = csv.lst[[1]], gr=gr, Gmat.pwy = G[,"pwy1",drop=F], name = NA,
           ntop = 7, plot = TRUE, seed = 0)
  expect_doppelganger("pwy1-ann", pp.ann)
})

test_that("negatives with decimals", {
  feat.tmp <- feat.tab
  feat.tmp$z <- 0.1*feat.tab$z
  pp.1x <- function() plot_pwy(feat.tab = feat.tmp, impact.tab = csv.lst[[1]], gr=gr, Gmat.pwy = G[,"pwy1",drop=F], name = NA,
                                 ntop = 7, plot = TRUE, seed = 0)
  expect_doppelganger(title="pwy1.1x", pp.1x)
})

test_that("analyte in G & but not feat.tab", {
  feat.tmp <- feat.tab[-1,]
  pp.na <- function() plot_pwy(feat.tab = feat.tmp, impact.tab = csv.lst[[1]], gr=gr, Gmat.pwy = G[,"pwy1",drop=F], name = NA,
                                ntop = 7, plot = TRUE, seed = 0)
  expect_doppelganger(title="pwy1-na", pp.na)
})
