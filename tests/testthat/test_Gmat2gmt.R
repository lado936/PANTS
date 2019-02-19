context("Gmat2gmt")

test_that("roundtrip G from helper", {
  expect_equal(gmt, Gmat2gmt(G))
})
