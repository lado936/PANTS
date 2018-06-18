context("score_features")

test_that("score_features", {
  sf <- score_features(object=M, phenotype.v=pheno, contrast.v="trt1-ctrl", score_fcn=abs)
  expect_equal(which.max(sf), c(a=1))
  
  f2 <- function(x, y) x+y
  expect_error(score_features(object=M, phenotype.v=pheno, contrast.v="trt1-ctrl", score_fcn=f2))
})
