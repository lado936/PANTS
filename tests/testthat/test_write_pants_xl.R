context("write pants xl")

test_that("w/o ker", {
  noker <- PANTS:::diag_kernel(object=M, Gmat=G)

  sv <- setNames(res.noker$feature.stats$score, nm=rownames(res.noker$feature.stats))
  wlx <- PANTS:::write_pants_xl(score.v=sv, pwy.tab=res.noker$pwy.stats, 
                                  feat.tab=res.noker$feature.stats, Gmat=G, ker=noker)
  
  expect_equal(grep("=HYPERLINK(", wlx[,1], fixed = TRUE), 1:nrow(wlx))
  expect_equal(wlx[,-1], res.noker$pwy.stats)
})

test_that("w ker", {
  sv <- setNames(res$feature.stats$score, nm=rownames(res$feature.stats))
  wlx <- PANTS:::write_pants_xl(score.v=sv, pwy.tab=res$pwy.stats, 
                                feat.tab=res$feature.stats, Gmat=G, ker=kk)
  
  expect_equal(grep("=HYPERLINK(", wlx[,1], fixed = TRUE), 1:nrow(wlx))
  expect_equal(wlx[,-1], res$pwy.stats)
})