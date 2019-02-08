#' Select top drivers for a pathway
#' 
#' Select \code{ntop} top drivers for a pathway.
#' 
#' @inheritParams pants
#' @inheritParams plot_pwy
#' @return Data frame with \code{ntop} rows ordered by impact & 3 columns: \code{node} with node names; 
#' \code{impact} with impact values; \code{in.pwy} with logicals if node is in \code{pwy}.

select_ntop <- function(score.v, Gmat, pwy, ker, alternative=c("two.sided", "less", "greater"), ntop=3){
  stopifnot(is.finite(score.v), !is.null(names(score.v)), length(pwy) == 1, pwy %in% colnames(Gmat), 
            !is.null(ker), ncol(ker) == nrow(Gmat), ncol(ker) == length(score.v), 
            colnames(ker) == names(score.v))
  alternative <- match.arg(alternative)
  
  if (ntop > length(score.v)) ntop <- length(score.v)
  
  #get kernel weight sums per node
  coeff.sc <- (ker %*% Gmat[,pwy])[,1]
  
  #estimate impact of nodes on pwy score
  #this includes estimation of impact.v to NA nodes, which are properly handled by order() below
  impact.v <- stats::setNames((coeff.sc * score.v), nm=names(score.v))
  
  #get top n nodes by abs(impact.v)
  nodes.o <- names(impact.v)[switch(alternative, 
                                    greater=order(-impact.v),
                                    less=order(impact.v), 
                                    two.sided=order(-abs(impact.v)))]
  top.node.nms <- nodes.o[1:ntop]
  in.pwy <- Gmat[top.node.nms, pwy] != 0
  top.nodes <- data.frame(node=top.node.nms, impact=impact.v[top.node.nms], in.pwy=in.pwy, stringsAsFactors=FALSE)
  return(top.nodes)
}