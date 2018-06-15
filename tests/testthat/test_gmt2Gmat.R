context("gmt2Gmat")

# G <- gmt2Gmat(gmt) #in helper

test_that("G from helper", {
  expect_true(all(G@x == 1))
  expect_equal(sum(G), 6)
  expect_equal(G["d", "pwy1"], 0)
  expect_equal(G["a", "pwy2"], 0)
})
