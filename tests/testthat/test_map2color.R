context("map2color")

test_that("SO example", {
  mypal <- colorRampPalette(c("blue", "red"))(5)
  x <- c(1, 9, 8.5, 3, 3.4, 6.2)
  y <- map2color(x, mypal)
  expect_equal(length(x), length(y))
  #verified that these colors are monotonic in x
  expect_equal(y, c("#0000FF", "#FF0000", "#FF0000", "#3F00BF", "#3F00BF", "#BF003F"))
})