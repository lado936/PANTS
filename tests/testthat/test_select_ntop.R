context("select ntop")

test_that("w/o ker", {
  noker <- PANTS:::diag_kernel(object=M, Gmat=G)
  
  sn <- PANTS:::select_ntop(score.v=score.v, Gmat=G, pwy="pwy1", ker=noker, ntop = 3)
  expect_equal(length(sn), 3)
})