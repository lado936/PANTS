context("graph2kernel")

# kk <- graph2kernel(gr) #in helper

test_that("edgelist -> kernel", {
  expect_equal(kk[2,2], 1)
  expect_true(isSymmetric(as.matrix(kk)))
  #B & D are equivalent
  expect_equal(kk["a", "b"], kk["a", "d"])
  #a <-> c not connected, so k=0
  expect_equal(kk["a", "c"], 0)
  #a=2, p=1 yields similarities
  expect_true(all(kk@x <= 1))
  expect_true(all(kk@x >= 0))
})

test_that("gr not simple", {
  #add redundant edge
  expect_message(kk2 <- graph2kernel(gr2))
  expect_equal(kk, kk2)
})