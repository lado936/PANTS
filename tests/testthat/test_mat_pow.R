context("mat_pow")

test_that("dense matrix", {
  #from https://www.mathworks.com/help/matlab/ref/mpower.html
  x <- matrix(1:4, nrow=2, byrow = TRUE)
  expect_equal(mat_pow(x, 2), matrix(c(7,10,15,22), nrow=2, byrow = TRUE))
  expect_equal(mat_pow(x, 3), x %*% x %*% x)
  expect_error(mat_pow(x, 1.5))
})

test_that("sparse matrix", {
  x <- Matrix(1:4, nrow=2, byrow = TRUE)
  expect_equal(mat_pow(x, 2), Matrix(c(7,10,15,22), nrow=2, byrow = TRUE))
})

test_that("bad x", {
  x <- matrix(1:6, nrow=3, byrow = TRUE)
  expect_error(mat_pow(x, 2))
  expect_error(mat_pow(x=1:3, 2))
})

test_that('mat_pow > 2', {
  mat <- matrix(1:9, nrow = 3)
  mat4pow <- mat %*% mat
  mat4pow <- mat4pow %*% mat
  mat4pow <- mat4pow %*% mat
  expect_equal(mat_pow(mat,4), mat4pow)
})