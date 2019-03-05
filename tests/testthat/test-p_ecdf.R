context("p_ecdf")

set.seed(0)
nf <- 3
nsim <- 1000
fnms <- paste0("feature", 1:nf)
eval.v <- setNames(c(0, 1, 0.5), nm=fnms)
score.mat <- matrix(runif(n=nf*nsim), nrow=nf, ncol=nsim, dimnames=list(fnms, paste0("sim", 1:nsim)))

test_that("alternatives", {
  pe1 <- p_ecdf(eval.v, score.mat, alternative = "greater")
  expect_gte(pe1[1, "p"], 0.99)
  expect_lte(pe1[2, "p"], 0.01)
  expect_lte(pe1[3, "p"], 0.7)
  expect_gte(pe1[3, "p"], 0.3)

  pe2 <- p_ecdf(eval.v, score.mat, alternative = "less")
  expect_gte(pe2[2, "p"], 0.99)
  expect_lte(pe2[1, "p"], 0.01)
  expect_lte(pe2[3, "p"], 0.7)
  expect_gte(pe2[3, "p"], 0.3)
  
  pe3 <- p_ecdf(eval.v, score.mat)
  expect_lte(pe3[1, "p"], 0.01)
  expect_lte(pe3[2, "p"], 0.01)
  expect_gte(pe3[3, "p"], 0.9)
})