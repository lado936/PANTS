context("map2color")

mypal <- colorRampPalette(c("blue", "red"))(5)

test_that("SO example", {
  x <- c(1, 9, 8.5, 3, 3.4, 6.2)
  y <- map2color(x, mypal)
  expect_equal(length(x), length(y))
  #verified that these colors are monotonic in x
  expect_equal(y, c("#0000FF", "#FF0000", "#FF0000", "#3F00BF", "#3F00BF", "#BF003F"))
})

test_that("x has NA", {
  x <- c(1, 9, 8.5, 3, NA, 6.2)
  y <- map2color(x, mypal)
  expect_true(is.na(y[is.na(x)]))
})