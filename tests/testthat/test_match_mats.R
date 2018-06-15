context("match_mats")

test_that("match_mats", {
  expect_gt(length(intersect(rownames(G), colnames(kk))), 0)
  expect_equal(sum(G), 6)
  
  M2 <- M[,pheno != 'trt2']
  pheno2 <- pheno[pheno != 'trt2']
  score_fcn <- identity
  contrast.v2 <- contrast.v[1]
  nperm <- 10
  score.v <- score_features(object= M2, phenotype.v= pheno2, contrast.v= contrast.v2, score_fcn= score_fcn )
  
  ##The following code snippet is from PANTS
  #feature scores in permutations, 74% dense but later combine with a sparse (empty) matrix
  score.mat <- Matrix::Matrix(0, nrow=nrow(M2), ncol=nperm, dimnames = list(rownames(M2), paste0('perm', 1:nperm)))
  for (perm in 1:nperm){
    #must set permuted names to NULL st limma_contrasts doesn't complain thay they clash with colnames(object)
    pheno.tmp <- stats::setNames(pheno2[sample(1:length(pheno2))], nm=NULL)
    
    score.mat[,perm] <- score_features(object=M2, phenotype.v=pheno.tmp, contrast.v=contrast.v2, score_fcn=score_fcn)
    
  }
  score.mat2 <- cbind(v=score.v, score.mat)
  mm <- match_mats(score.mat = score.mat2, ker=kk, Gmat=G)
  ##End
  
  expect_equal(dimnames(mm$score.mat)[1],dimnames(mm$ker)[1])
  expect_equal(dimnames(mm$score.mat)[1],dimnames(mm$Gmat)[1])
  expect_equal(mm$ker,kk[rownames(mm$ker),colnames(mm$ker)])
  expect_equal(mm$score.mat,score.mat2[rownames(mm$score.mat),colnames(mm$score.mat)])
  expect_equal(mm$Gmat,G[rownames(mm$Gmat),colnames(mm$Gmat)])

})