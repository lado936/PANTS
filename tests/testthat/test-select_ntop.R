context("select ntop")

test_that("ker x alternative", {
  #noker & 2-sided
  sn <- select_ntop(zscore.v=zscore.v, Gmat=G, pwy="pwy1", ker=noker, ntop = 3)
  expect_equal(nrow(sn), 3)
  expect_true(all(sn$in.pwy))

  #ker & 2-sided
  #a isn't in pwy2 but impacts it
  sn.z <- select_ntop(zscore.v=zscore.v, Gmat=G, pwy="pwy2", ker=kk, ntop = 3)
  impact.v <- (kk %*% G[,"pwy2"])[,1] * zscore.v
  expect_equal(impact.v[rownames(sn.z)], setNames(sn.z[, "impact"], nm=rownames(sn.z)))
  
  #noker & 1-sided
  #shouldn't inc. node d: it's outside pwy & there's no kernel
  sn.z <- select_ntop(zscore.v=zscore.v, Gmat=G, pwy="pwy1", ker=noker, ntop = 4, alternative = "greater")
  expect_equal(nrow(sn.z), 3)
  impact.v <- (noker %*% G[,"pwy1"])[,1] * zscore.v
  expect_equal(impact.v[rownames(sn.z)], setNames(sn.z[, "impact"], nm=rownames(sn.z)))
  
  #ker & 1-sided
  sn.z <- select_ntop(zscore.v=zscore.v, Gmat=G, pwy="pwy1", ker=kk, ntop = 4, alternative = "less")
  impact.v <- (kk %*% G[,"pwy1"])[,1] * zscore.v
  expect_equal(impact.v[rownames(sn.z)], setNames(sn.z[, "impact"], nm=rownames(sn.z)))
})

test_that("impact = Ki*Gj*zi", {
  sn.sc <- select_ntop(zscore.v=score.v, Gmat=G, pwy="pwy1", ker=kk, ntop = 4)
  sn.z <- select_ntop(zscore.v=zscore.v, Gmat=G, pwy="pwy1", ker=kk, ntop = 4)
  expect_false(any(sn.z$impact == sn.sc[rownames(sn.sc), "impact"]))
  
  impact.v <- (kk %*% G[,"pwy1"])[,1] * zscore.v
  expect_equal(impact.v, setNames(sn.z[names(impact.v), "impact"], nm=names(impact.v)))
})
