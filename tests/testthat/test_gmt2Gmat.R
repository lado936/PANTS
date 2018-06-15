context("gmt2Gmat")

test_that("gmt2Gmat", {
  expect_true(all(G@x == 1))
  expect_equal(sum(G), 6)
  expect_equal(G[4,1], 0)
  expect_equal(G[1,2], 0)
})