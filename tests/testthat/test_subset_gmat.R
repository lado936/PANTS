context("subset_gmat")

test_that("min.size", {
  zeallot::`%<-%`(c(g, npp), subset_gmat(object=M, Gmat=G[1:3,], min.size=3))
  expect_equal(as.numeric(g), rep(1, 3))
  expect_equal(npp, c(pwy1=3))
  
  zeallot::`%<-%`(c(g, npp), subset_gmat(object=M, Gmat=G[1:3,], min.size=0))
  expect_equal(as.numeric(g), c(1,1,1,0,1,1))
  expect_equal(npp, c(pwy1=3, pwy2=2))
  
  expect_error(subset_gmat(object=M, Gmat=G[1:3,], min.size=4))
})