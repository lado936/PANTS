context("ez arrangments")

test_that("ezpermutations", {
  xx <- c("a", "a", "b")
  ezc1 <- ezpermutations(xx=xx, nperm=1000, freq=2)
  expect_false(any(duplicated( data.frame(Reduce(rbind, list(ezc1, list(xx=list(xx=xx))))) )))
  expect_equal(length(ezc1), 5)
  
  ezc2 <- ezpermutations(xx=xx, nperm=1000, freq=c(2, 1))
  expect_false(any(duplicated( data.frame(Reduce(rbind, c(ezc2, list(xx=xx)))) )))
  expect_equal(length(ezc2), 2)
  
  xx <- c("a", "b", "c")
  ezc4 <- ezpermutations(xx, nperm=4, freq=2)
  expect_false(any(duplicated( data.frame(Reduce(rbind, c(ezc4, list(xx=xx)))) )))
  expect_equal(length(ezc4), 4)
  
  ezc5 <- ezpermutations(xx, nperm=1000, freq=2)
  expect_false(any(duplicated(c(xx, ezc5))))
  expect_equal(length(ezc5), factorial(3)-1)
  
  ezc6 <- ezpermutations(xx=xx, nperm=1000, freq=1)
  expect_false(any(duplicated( data.frame(Reduce(rbind, c(ezc6, list(xx=xx)))) )))
  
  xx <- c("a", "a", "b", "c", "d")
  ezc10 <- ezpermutations(xx=xx, nperm=100, freq=2)
  expect_false(any(duplicated( data.frame(Reduce(rbind, c(ezc10, list(xx=xx)))) )))
  expect_equal(length(ezc10), 100)
  
  xx <- 1:length(pheno)
  ezc20 <- ezpermutations(xx=xx, nperm=50, freq=1)
  expect_false(any(duplicated( data.frame(Reduce(rbind, c(ezc20, list(xx=xx)))) )))
  expect_equal(length(ezc20), 50)
  
  # nperm > 10000
  ezc21 <- ezpermutations(xx=xx, nperm=11000, freq=1)
  expect_false(any(duplicated( data.frame(Reduce(rbind, c(ezc21, list(xx=xx)))) )))
  expect_equal(length(ezc21), 11000)
})