#' Select top drivers for a pathway
#' 
#' Select \code{ntop} top drivers for a pathway by calculating impact, sorting nodes with \code{alternative}, and selecting top nodes.
#' 
#' @inheritParams pants
#' @inheritParams plot_pwy
#' @inheritParams ezlimma::roast_contrasts
#' @details Independent of the alternative, pathway significance is most affected by nodes with largest magnitude impact,
#' however these nodes are sorted in the output dependent on the alternative. Nodes outside the pathway with no impact, 
#' which may arise when the kernel is diagonal, are removed.
#' @return Data frame with \code{ntop} rows ordered by impact & 3 columns: \code{node} with node names; 
#' \code{impact} with impact values; \code{in.pwy} with logicals if node is in \code{pwy}.

select_ntop <- function(zscore.v, Gmat, pwy, ker, alternative=c("two.sided", "less", "greater"), ntop=3){
  stopifnot(is.na(zscore.v) | is.finite(zscore.v), !is.null(names(zscore.v)), length(pwy) == 1, pwy %in% colnames(Gmat), 
            !is.null(ker), ncol(ker) == nrow(Gmat), ncol(ker) == length(zscore.v), 
            colnames(ker) == names(zscore.v))
  alternative <- match.arg(alternative)
  
  if (ntop > length(zscore.v)) ntop <- length(zscore.v)
  
  # get kernel weight sums per node
  coeff.sc <- (ker %*% Gmat[,pwy])[,1]
  
  # estimate impact of nodes on pwy score
  # this includes estimation of impact.v to NA nodes, which are properly handled by order() below
  # want Ki*Gj*zi = coeff.sc * each element of z, so can multiply elementwise
  impact.v <- stats::setNames((coeff.sc * zscore.v), nm=names(zscore.v))
  # always want N largest in magnitude, even for 1-sided test, since these impact most, even if in wrong direction
  impact.v <- impact.v[order(-abs(impact.v))][1:ntop]
  # order based on alternative
  impact.o <- impact.v[switch(alternative, 
                              greater=order(-impact.v),
                              less=order(impact.v), 
                              two.sided=order(-abs(impact.v)))]
  top.node.nms <- names(impact.o)
  
  in.pwy <- Gmat[top.node.nms, pwy] != 0
  top.nodes <- data.frame(node=top.node.nms, impact=impact.v[top.node.nms], in.pwy=in.pwy, stringsAsFactors=FALSE)
  top.nodes <- top.nodes[top.nodes$impact != 0 | top.nodes$in.pwy,]
  
  return(top.nodes)
}