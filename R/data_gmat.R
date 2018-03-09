#' Pathway inclusion matrix (Gmat) constructed from SMPDB.
#'
#' To construct this, I read in 'smpdb_metabolites.csv' and 'smpdb_proteins.csv' from the Small Molecule Pathway
#' Database (SMPDB), then SMPDB2Gmat(smpdb.prot=smpdb.prot, smpdb.met=smpdb.met, exclude.pwy.subj='Drug Metabolism').
#'
#' @format A Matrix of features (proteins/metabolites) by Pathway names. Zero entries indicate the feature is not in the pathway,
#' whereas non-zero entries indicates otherwise.
"gmat"
