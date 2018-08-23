context("match_mats")

test_that("helper objects", {
  #rm rows so match_mats needs to add them back
  M <- M[setdiff(rownames(M), "c"),]
  G <- G[setdiff(rownames(G), "b"),]
  kk <- kk[setdiff(rownames(kk), "a"), setdiff(rownames(kk), "a")]
  
  mm <- match_mats(score.mat=M, ker=kk, Gmat=G, score.impute = 1)
  M2 <- mm$score.mat; k2 <- mm$ker; G2 <- mm$Gmat
  
  expect_true("c" %in% rownames(M2))
  expect_true(all(M2["c",]==1))
  
  expect_equal(rownames(M2), rownames(k2))
  expect_equal(rownames(M2), rownames(G2))
  
  k.int <- intersect(rownames(kk), rownames(k2))
  expect_equal(k2[k.int, k.int], kk[k.int, k.int])
  
  g.int <- intersect(rownames(G), rownames(G2))
  expect_equal(G2[g.int,], G[g.int,])
  
  m.int <- intersect(rownames(M2), rownames(M))
  expect_equal(M[m.int,], as.matrix(M2[m.int,]))
})