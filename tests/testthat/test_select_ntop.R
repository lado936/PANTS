context("select ntop")

test_that("ker x alternative", {
  #noker & 2-sided
  sn <- select_ntop(score.v=score.v, Gmat=G, pwy="pwy1", ker=noker, ntop = 3)
  expect_equal(nrow(sn), 3)
  expect_true(all(sn$in.pwy))

  #ker & 2-sided
  #a isn't in pwy2 but impacts it
  sn <- select_ntop(score.v=score.v, Gmat=G, pwy="pwy2", ker=kk, ntop = 3)
  expect_equal(sn[1,1], "a")
  expect_equal(sn[2,1], "d")
  
  #noker & 1-sided
  #shouldn't inc. node d: it's outside pwy & there's no kernel
  sn <- select_ntop(score.v=score.v, Gmat=G, pwy="pwy1", ker=noker, ntop = 4, alternative = "greater")
  expect_equal(nrow(sn), 3)
  expect_equal(sn$node, c("a", "c", "b"))
  
  #ker & 1-sided
  sn <- select_ntop(score.v=score.v, Gmat=G, pwy="pwy1", ker=kk, ntop = 4, alternative = "less")
  expect_equal(sn$node, c("b", "c", "d", "a"))
})