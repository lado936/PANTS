context("write pants xl")

test_that("w/o ker", {
  sv <- setNames(res.noker$feature.stats$score, nm=rownames(res.noker$feature.stats))
  wpx <- write_pants_xl(score.v=sv, pwy.tab=res.noker$pwy.stats, feat.tab=res.noker$feature.stats, Gmat=G, 
                                ker=noker, alternative="two.sided", name="test_wpx")
  
  expect_equal(grep("=HYPERLINK(", wpx[,1], fixed = TRUE), 1:nrow(wpx))
  expect_equal(wpx[,-1], res.noker$pwy.stats)
})

test_that("w ker", {
  sv <- setNames(res$feature.stats$score, nm=rownames(res$feature.stats))
  pwy.tab <- res$pwy.stats
  rownames(pwy.tab)[2] <- colnames(G)[2] <- "pwy2."
  wpx <- write_pants_xl(score.v=sv, pwy.tab=pwy.tab, feat.tab=res$feature.stats, Gmat=G, ker=kk, 
                        alternative="two.sided", name="test_wpx")
  expect_equal(grep("=HYPERLINK(", wpx[,1], fixed = TRUE), 1:nrow(wpx))
  
  rownames(wpx)[2] <- "pwy2"
  expect_equal(wpx[,-1], res$pwy.stats)
  
  expect_true(file.exists("test_wpx/test_wpx.xlsx"))
  expect_true(file.exists("test_wpx/pathways/pwy1.csv"))
  expect_true(file.exists("test_wpx/pathways/pwy2_.csv"))
})

teardown({
  unlink("test_wpx", recursive = TRUE)
})