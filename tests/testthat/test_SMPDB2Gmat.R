context("SMPDB2Gmat")

smpdb.met <- data.frame(Pathway.Name=c("Alanine Metabolism", "Aspartate Metabolism", "Aspartate Metabolism", 
                        "Refsum Disease"), Pathway.Type=c("Metabolic", "Metabolic", "Metabolic", "Disease"),
                        ChEBI.ID=c("15956", "15366", "16027", "15351"), stringsAsFactors = FALSE)
smpdb.prot <- data.frame(Pathway.Name=c("Alanine Metabolism", "Aspartate Metabolism", "Aspartate Metabolism", 
                        "Primary Hyperoxaluria Type"), Pathway.Subject=c("Metabolic", "Metabolic", "Metabolic", 
                        "Disease"), Gene.Name=c("AARS", "ABAT", "ASL", "AARS"), stringsAsFactors = FALSE)

test_that("example subset", {
  g <- SMPDB2Gmat(smpdb.prot=smpdb.prot, smpdb.met=smpdb.met, exclude.pwy.subj="Disease")
  expect_true(all(colSums(as.matrix(g))==c(2, 4)))
  expect_equal(g["AARS", "Alanine Metabolism"], 1)
  expect_equal(g["CHEBI:15956", "Alanine Metabolism"], 1)
})
