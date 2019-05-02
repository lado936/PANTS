context("diag_kernel")

test_that("helper example", {
  dk <- diag_kernel(object.rownames = rownames(M)[-1], Gmat.rownames = rownames(G))
  expect_equal(as.matrix(dk), diag(3))
  expect_equal(dimnames(dk), list(letters[2:4], letters[2:4]))
})