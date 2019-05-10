context("ez arrangments")

test_that("ezpermutations", {
  xx <- c("a", "a", "b")
  ezc1 <- ezpermutations(xx=xx, nperm=1000, freq=2)
  expect_false(any(duplicated( cbind(ezc1, xx=xx) )))
  expect_equal(length(ezc1), 5)
  
  ezc2 <- ezpermutations(xx=xx, nperm=1000, freq=c(2, 1))
  expect_false(any(duplicated( cbind(ezc2, xx) )))
  expect_equal(length(ezc2), 2)
  
  xx <- c("a", "b", "c")
  ezc4 <- ezpermutations(xx, nperm=4, freq=2)
  expect_false(any(duplicated( cbind(ezc4, xx) )))
  expect_equal(length(ezc4), 4)
  
  ezc5 <- ezpermutations(xx, nperm=1000, freq=2)
  expect_false(any(duplicated(c(xx, ezc5))))
  expect_equal(length(ezc5), factorial(3)-1)
  
  ezc6 <- ezpermutations(xx=xx, nperm=1000, freq=1)
  expect_false(any(duplicated( cbind(ezc6, xx) )))
  
  xx <- c("a", "a", "b", "c", "d")
  ezc10 <- ezpermutations(xx=xx, nperm=100, freq=2)
  expect_false(any(duplicated( cbind(ezc10, xx) )))
  expect_equal(length(ezc10), 100)
  
  xx <- 1:length(pheno)
  ezc20 <- ezpermutations(xx=xx, nperm=50, freq=1)
  expect_false(any(duplicated( cbind(ezc20, xx) )))
  expect_equal(length(ezc20), 50)
  
  # nperm > 10000
  ezc21 <- ezpermutations(xx=xx, nperm=11000, freq=1)
  expect_false(any(duplicated( cbind(ezc21, xx) )))
  expect_equal(length(ezc21), 11000)
  
  # maintain type: num
  ezc30 <- ezpermutations(xx=1:9, nperm=12, freq=1)
  ret.num <- all(sapply(ezc30, FUN=is.numeric))
  expect_true(ret.num)
  expect_false(all(1:9 == ezc30[[1]]))
  
  xx <- 1:20
  ezc31 <- ezpermutations(xx=xx, nperm=12, freq=1)
  expect_true(all(sapply(ezc31, FUN=is.numeric)))
  expect_false(any(duplicated( cbind(ezc31, xx=xx) )))
})
  