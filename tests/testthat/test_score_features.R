context("score_features")

test_that("score_features", {
  sf <- score_features(object=M, phenotype.v=pheno, contrast.v="trt1-ctrl", score_fcn=abs)
  expect_equal(which.max(sf), c(a=1))
  
  f2 <- function(x) return(c(sum(x), sum(x)))
  expect_error(score_features(object=M, phenotype.v=pheno, contrast.v=contrast.v, score_fcn=f2))
})
