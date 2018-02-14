#'Draw network diagram for a pathway
#'
#'Draw network diagram for a pathway with node size corresponding to significance and color to pathway membership.
#'
#'@param gr A graph of class 'igraph'
#'@param ker Kernel matrix, can be sparse matrix from package \code{Matrix}.
#'@param Gmat Pathway membership matrix, can be sparse matrix from package \code{Matrix}.
#'@param pwy Pathway to plot. Must be a column name of \code{Gmat}.
#'@param pval.v Namd vector of p-values of features. \code{names(pval.v)} should equal \code{rownames(net)}.
#'@param annot Named vector of annotations for nodes. If \code{annot} is not \code{NA}, \code{names(annot)} should
#'overlap with \code{rownames(Gmat)}.
#'@param ntop Number of top most significant features to include. If one of these is an external node, then its
#'internal neighbor nodes are also included. These nodes are then connected based on the network.
#'@param in.col Color for nodes in a pathway.
#'@param out.col Color for nodes outside of a pathway.
#'@param name.pdf Name for PDF file to plot to. Can't contain characters ":" or "/" on Windows. Set to \code{NA}
#'to suppress writing to file.
#'@return Invisibly, a list of 3 components: 
#' \describe{
#'   \item{gr}{the graph that gets plotted}
#'   \item{vertex.color}{the vertex colors}
#'   \item{vertex.size}{the vertex sizes}
#' }
#'@export

draw_pwy_net <- function(gr, ker, Gmat, pwy, pval.v, annot=NA, ntop=7, in.col='lightblue', out.col='red',
                         name.pdf=paste0(gsub(":|/", "_", pwy), '_ntop', ntop)){
  
  if (!igraph::is_simple(gr)){
    warning('igraph::is_simple(gr) is FALSE, so applying igraph::simplify.')
    gr <- igraph::simplify(gr)
  }
  
  pwy.nodes <- rownames(Gmat)[Gmat[,pwy]>0]

  pval.match <- setNames(rep(median(pval.v), nrow(ker)), nm=rownames(ker))
  pval.match[names(pval.v)] <- pval.v

  stopifnot(names(pval.v)==rownames(gr), colnames(ker)==rownames(Gmat),
            names(pval.v) %in% rownames(ker), is.na(annot)|length(intersect(names(annot), rownames(Gmat))) > 0)

  pwy.neighbors <- setdiff(neighbor_nms(gr, pwy.nodes), pwy.nodes)

  #get kernel weight sums per node
  ker.v0 <- ker %*% Gmat[,pwy]
  #want mean of ker weights to be 1 to standardize threshold, but want to keep weights non-neg,
  #so divide by mean (instead of subtracting mean)
  ker.v <- setNames(as.numeric(ker.v0 / mean(ker.v0[,1])), nm=rownames(Gmat))
  #names(ker.v) <- rownames(Gmat)

  #limit estimation of impact.v to nodes that have data
  impact.v <- setNames(rep(0, nrow(ker)), nm=rownames(ker))
  impact.v[names(pval.v)] <- ker.v[names(pval.v)] * qnorm(p=pval.v, lower.tail=FALSE)

  #get top n nodes
  top.nodes <- names(impact.v)[order(impact.v, decreasing = TRUE)][1:ntop]

  #get neighbors
  pwy.neighbors.ss <- intersect(pwy.neighbors, top.nodes)
  #get pwy nodes with large stats OR pwy nodes connected to neighbors with large stats
  pwy.nodes.ss <- union(intersect(pwy.nodes, top.nodes),
                        intersect(pwy.nodes, neighbor_nms(gr, pwy.neighbors.ss)))

  gr.ss <- igraph::induced_subgraph(gr, vid=which(V(gr)$name %in% c(pwy.nodes.ss, pwy.neighbors.ss)))

  color.v <- c(out.col, in.col)[(V(gr.ss)$name %in% pwy.nodes)+1]
  names(color.v) <- V(gr.ss)$name
  #arbitrary monotonic transformation
  sz.v <- 2+qchisq(p=pval.match[V(gr.ss)$name], df=4, lower.tail=FALSE)

  #sub chebi id's for names
  if (!is.na(annot)){
    nms.int <- intersect(names(annot), V(gr.ss)$name)
    if (length(nms.int) > 0){
      V(gr.ss)$name[match(nms.int, V(gr.ss)$name)] <- annot[nms.int]
    }
  }

  #need to gsub disallowed characters
  if (!is.na(name.pdf)) pdf(paste0(name.pdf, '.pdf'))
  plot(gr.ss, vertex.color=color.v, vertex.size=sz.v)
  if (!is.na(name.pdf)) dev.off()

  ret <- list(gr=gr.ss, vertex.color=color.v, vertex.size=sz.v)
  return(invisible(ret))
}