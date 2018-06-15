context("match_mats")

test_that("match_mats", {
  expect_gt(length(intersect(rownames(G), colnames(kk))), 0)
  expect_equal(sum(G), 6)
})