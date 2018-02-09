library("PANTS")
context("utils")

test_that("mat_pow on dense matrix", {
  #from https://www.mathworks.com/help/matlab/ref/mpower.html
  x <- matrix(1:4, nrow=2, byrow = TRUE)
  expect_equal(mat_pow(x, 2), matrix(c(7,10,15,22), nrow=2, byrow = TRUE))
})

test_that("mat_pow on sparse matrix", {
  x <- Matrix(1:4, nrow=2, byrow = TRUE)
  expect_equal(mat_pow(x, 2), Matrix(c(7,10,15,22), nrow=2, byrow = TRUE))
})