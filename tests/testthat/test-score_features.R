context("score_features")

test_that("contr", {
  sf <- score_features(object=M, phenotype=pheno, contrast.v="trt1-ctrl", score_fcn=abs)
  expect_equal(which.max(sf), c(a=1))
  
  f2 <- function(x) return(c(sum(x), sum(x)))
  expect_error(score_features(object=M, phenotype=pheno, contrast.v=contrast.v, score_fcn=f2))
})

test_that("design: contr vs cor", {
  M.tmp <- M[,1:6]
  ph.tmp <- pheno[1:6]
  ph.num <- as.numeric(ph.tmp=="trt1")
  covar <- rnorm(length(ph.tmp))
  contr <- c(diff="trt1-trt2")
  
  des2 <- model.matrix(~0+ph.tmp+covar)
  colnames(des2) <- gsub("ph.tmp", "", colnames(des2))
  sc2.v <- score_features(object=M.tmp, design = des2, contrast.v = contr, type="contrasts")
  
  des.lc <- model.matrix(~1+ph.num+covar)
  sc.lc <- score_features(object=M.tmp, design = des.lc, type="cor")
  
  expect_equal(sc2.v, sc.lc)
})