context("write pants xl")

test_that("w/o ker", {
  zv <- res.noker$feature.stats[,"z",drop=FALSE]
  wpx <- write_pants_xl(zscores=zv, pwy.tab=res.noker$pwy.stats, feat.tab=res.noker$feature.stats, Gmat=G, 
                                ker=noker, name="test_wpx")[[1]]
  expect_equal(grep("=HYPERLINK(", wpx[,1], fixed = TRUE), 1:nrow(wpx))
  expect_equal(wpx[,-1], res.noker$pwy.stats)
})

test_that("w ker", {
  zv <- feature.stats[,"z",drop=FALSE]
  pwy.tab <- pwy.stats
  rownames(pwy.tab)[2] <- colnames(G)[2] <- "pwy2."
  wpx <- write_pants_xl(zscores=zv, pwy.tab=pwy.tab, feat.tab=feature.stats, Gmat=G, ker=kk, 
                        name="test_wpx")[[1]]
  expect_equal(grep("=HYPERLINK(", wpx[,1], fixed = TRUE), 1:nrow(wpx))
  
  rownames(wpx)[2] <- "pwy2"
  expect_equal(wpx[,-1], pwy.stats)
  
  expect_true(file.exists("test_wpx/test_wpx.xlsx"))
  expect_true(file.exists("test_wpx/pathways/pwy1.csv"))
  expect_true(file.exists("test_wpx/pathways/pwy2_.csv"))
})

teardown({
  unlink("test_wpx", recursive = TRUE)
})