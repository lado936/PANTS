context("diag_kernel")

test_that("helper example", {
  dk <- diag_kernel(object=M[-1,], Gmat=G)
  expect_equal(as.matrix(dk), diag(3))
  expect_equal(dimnames(dk), list(letters[2:4], letters[2:4]))
})