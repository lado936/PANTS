context("write pants xl")

test_that("w/o ker", {
  unlink("test_wpx", recursive = TRUE) #in case it already exists
  
  noker <- diag_kernel(object=M, Gmat=G)
  sv <- setNames(res.noker$feature.stats$score, nm=rownames(res.noker$feature.stats))
  wlx <- write_pants_xl(score.v=sv, pwy.tab=res.noker$pwy.stats, feat.tab=res.noker$feature.stats, Gmat=G, 
                                ker=noker, alternative="two.sided", name="test_wpx")
  
  expect_equal(grep("=HYPERLINK(", wlx[,1], fixed = TRUE), 1:nrow(wlx))
  expect_equal(wlx[,-1], res.noker$pwy.stats)
  unlink("test_wpx", recursive = TRUE)
})

test_that("w ker", {
  unlink("test_wpx", recursive = TRUE) #in case it already exists
  
  sv <- setNames(res$feature.stats$score, nm=rownames(res$feature.stats))
  wlx <- write_pants_xl(score.v=sv, pwy.tab=res$pwy.stats, feat.tab=res$feature.stats, Gmat=G, ker=kk, 
                        alternative="two.sided", name="test_wpx")
  
  expect_equal(grep("=HYPERLINK(", wlx[,1], fixed = TRUE), 1:nrow(wlx))
  expect_equal(wlx[,-1], res$pwy.stats)
  
  expect_true(file.exists("test_wpx/test_wpx.xlsx"))
  expect_true(file.exists("test_wpx/pathways/pwy1.csv"))
  expect_true(file.exists("test_wpx/pathways/pwy2.csv"))
  
  unlink("test_wpx", recursive = TRUE)
})