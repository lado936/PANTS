#' Select top drivers for a pathway
#' 
#' Select \code{ntop} top drivers for a pathway by calculating impact, sorting nodes with \code{alternative}, 
#' and selecting top nodes.
#' 
#' @param zscores Matrix with rownames as feature names, and 1st column with z-scores to select high impact nodes. 
#' Non-\code{NA} values should be finite.
#' @param pwy Pathway, must be a column name of \code{Gmat}.
#' @inheritParams pants
#' @inheritParams ezlimma::roast_contrasts
#' @details Independent of the alternative, pathway significance is most affected by nodes with largest magnitude impact,
#' however these nodes are sorted in the output according to alternative="greater". 
#' Nodes outside the pathway with no impact, which may arise when the kernel is diagonal, are removed.
#' @return Data frame with \code{ntop} rows ordered by impact & 3 columns: \code{node} with node names; 
#' \code{impact} with impact values; \code{in.pwy} with logicals if node is in \code{pwy}.

select_ntop_per_pwy <- function(zscores, Gmat, pwy, ker, ntop=3){
  zscore.v <- stats::setNames(zscores[,1], nm=rownames(zscores))
  stopifnot(is.na(zscore.v) | is.finite(zscore.v), !is.null(names(zscore.v)), length(pwy) == 1, pwy %in% colnames(Gmat), 
            !is.null(ker), ncol(ker) == nrow(Gmat), ncol(ker) == length(zscore.v), colnames(ker) == names(zscore.v))
  
  if (ntop > length(zscore.v)) ntop <- length(zscore.v)
  
  # get kernel weight sums per node
  coeff.sc <- (ker %*% Gmat[,pwy])[,1]
  
  # estimate impact of nodes on pwy score
  # this includes estimation of impact.v to NA nodes, which are properly handled by order() below
  # want Ki*Gj*zi = coeff.sc * each element of z, so can multiply elementwise
  impact.v <- stats::setNames((coeff.sc * zscore.v), nm=names(zscore.v))
  # always want N largest in magnitude, even for 1-sided test, since these impact most, even if in wrong direction
  impact.v <- impact.v[order(-abs(impact.v))][1:ntop]
  # order based on alternative="greater"
  impact.o <- impact.v[order(-impact.v)]
  top.node.nms <- names(impact.o)
  
  in.pwy <- Gmat[top.node.nms, pwy] != 0
  top.nodes <- data.frame(node=top.node.nms, impact=impact.v[top.node.nms], in.pwy=in.pwy, stringsAsFactors=FALSE)
  top.nodes <- top.nodes[top.nodes$impact != 0 | top.nodes$in.pwy,]
  
  return(top.nodes)
}