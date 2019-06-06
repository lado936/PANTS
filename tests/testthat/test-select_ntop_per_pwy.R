context("select ntop")

test_that("ker", {
  #noker
  sn <- select_ntop_per_pwy(zscores=feature.stats[, "z", drop=FALSE], Gmat=G, pwy="pwy1", ker=noker, ntop = 3)
  expect_equal(nrow(sn), 3)
  expect_true(all(sn$in.pwy))

  #ker
  #a isn't in pwy2 but impacts it
  sn.z <- select_ntop_per_pwy(zscores=feature.stats[, "z", drop=FALSE], Gmat=G, pwy="pwy2", ker=kk, ntop = 3)
  impact.v <- (kk %*% G[,"pwy2"])[,1] * feature.stats[, "z"]
  expect_equal(impact.v[rownames(sn.z)], setNames(sn.z[, "impact"], nm=rownames(sn.z)))
  
  #noker
  #shouldn't inc. node d: it's outside pwy & there's no kernel
  sn.z <- select_ntop_per_pwy(zscores=feature.stats[, "z", drop=FALSE], Gmat=G, pwy="pwy1", ker=noker, ntop = 4)
  expect_equal(nrow(sn.z), 3)
  impact.v <- (noker %*% G[,"pwy1"])[,1] * feature.stats[, "z", drop=FALSE]
  expect_equal(impact.v[rownames(sn.z),], sn.z[, "impact"])
})

test_that("impact = Ki*Gj*zi", {
  sn.sc <- select_ntop_per_pwy(zscores=feature.stats[, "score",drop=FALSE], Gmat=G, pwy="pwy1", ker=kk, ntop = 4)
  sn.z <- select_ntop_per_pwy(zscores=feature.stats[, "z", drop=FALSE], Gmat=G, pwy="pwy1", ker=kk, ntop = 4)
  expect_false(any(sn.z$impact == sn.sc[rownames(sn.sc), "impact"]))
  
  impact.v <- (kk %*% G[,"pwy1"])[,1] * feature.stats[, "z"]
  expect_equal(impact.v, setNames(sn.z[names(impact.v), "impact"], nm=names(impact.v)))
})
