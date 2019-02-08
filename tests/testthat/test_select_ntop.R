context("select ntop")

test_that("w/o ker", {
  noker <- diag_kernel(object=M, Gmat=G)
  
  sn <- select_ntop(score.v=score.v, Gmat=G, pwy="pwy1", ker=noker, ntop = 3)
  expect_equal(nrow(sn), 3)
})

test_that("w ker", {
  #a isn't in pwy2 but impacts it
  sn <- select_ntop(score.v=score.v, Gmat=G, pwy="pwy2", ker=kk, ntop = 3)
  expect_equal(sn[1,1], "a")
})