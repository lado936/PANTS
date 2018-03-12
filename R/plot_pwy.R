#'Plot network diagram for a pathway
#'
#'Plot network diagram for a pathway with node size corresponding to significance and color to pathway membership.
#'
#'@param gr A graph of class \code{igraph} representing the interaction network.
#'@param ker Kernel matrix, can be sparse matrix from package \code{Matrix}.
#'@param Gmat Pathway membership matrix, can be sparse matrix from package \code{Matrix}.
#'@param pwy Pathway to plot. Must be a column name of \code{Gmat}.
#'@param score.v Namd vector of scores of features, where \code{names(score.v) == rownames(gr)}.
#'@param annot Named vector of annotations for nodes. If \code{annot} is not \code{NA}, \code{names(annot)} should
#'overlap with \code{rownames(Gmat)}.
#'@param ntop Number of top most significant features to include. If one of these is an external node, then its
#'internal neighbor nodes are also included. These nodes are then connected based on the network.
#'@param name.pdf Name for PDF file to plot to. Can't contain characters ":" or "/" on Windows. Set to \code{NA}
#'to suppress writing to file.
#'@param color.pal Color palette.
#'@return Invisibly, a list of 3 components: 
#' \describe{
#'   \item{gr}{the graph that gets plotted}
#'   \item{vertex.color}{the vertex colors}
#'   \item{vertex.size}{the vertex sizes}
#' }
#'@export

plot_pwy <- function(gr, ker, Gmat, pwy, score.v, annot=NA, ntop=7, in.col='lightblue', out.col='red',
                         name.pdf=paste0(gsub(":|/", "_", pwy), '_ntop', ntop), color.v=NULL){
  
  if (!igraph::is_simple(gr)){
    warning('igraph::is_simple(gr) is FALSE, so applying igraph::simplify.')
    gr <- igraph::simplify(gr)
  }
  if (is.null(color.v)){
    if (any(score.v < 0)){
      color.v <- RColorBrewer::brewer.pal(n=9, name='RdBu')[9:1]
    } else {
      color.v <- RColorBrewer::brewer.pal(n=9, name='Reds')
    }
  }
  
  pwy.nodes <- rownames(Gmat)[Gmat[,pwy]>0]

  score.nona <- score.v
  score.nona[is.na(score.v)] <- median(score.v, na.rm = TRUE)
  # score.match <- setNames(rep(NA, nrow(ker)), nm=rownames(ker))
  # score.match[names(score.v)] <- score.v
  score.match <- score.nona[rownames(ker)]

  stopifnot(names(score.v)==rownames(gr), rownames(Gmat) %in% colnames(ker),
            names(score.v) %in% rownames(ker), is.na(annot)|length(intersect(names(annot), rownames(Gmat))) > 0)

  pwy.neighbors <- setdiff(neighbor_nms(gr, pwy.nodes), pwy.nodes)

  #get kernel weight sums per node
  ker.v0 <- ker %*% Gmat[,pwy]
  #want mean of ker weights to be 1 to standardize threshold, but want to keep weights non-neg,
  #so divide by mean (instead of subtracting mean)
  ker.v <- setNames(as.numeric(ker.v0 / mean(ker.v0[,1])), nm=rownames(Gmat))
  #names(ker.v) <- rownames(Gmat)

  #estimate impact of nodes on pwy score
  #limit estimation of impact.v to nodes that have data
  impact.v <- setNames(rep(0, nrow(ker)), nm=rownames(ker))
  impact.v[names(score.v)] <- ker.v[names(score.v)] * score.v

  #get top n nodes by abs(impact.v)
  top.nodes <- names(impact.v)[order(abs(impact.v), decreasing = TRUE)][1:ntop]

  #get neighbors
  pwy.neighbors.ss <- intersect(pwy.neighbors, top.nodes)
  #get pwy nodes with large stats OR pwy nodes connected to neighbors with large stats
  pwy.nodes.ss <- union(intersect(pwy.nodes, top.nodes),
                        intersect(pwy.nodes, neighbor_nms(gr, pwy.neighbors.ss)))

  gr.ss <- igraph::induced_subgraph(gr, vid=which(V(gr)$name %in% c(pwy.nodes.ss, pwy.neighbors.ss)))

  shape.v <- c(out.sh, in.sh)[(V(gr.ss)$name %in% pwy.nodes)+1]
  names(shape.v) <- V(gr.ss)$name
  #arbitrary monotonic transformation
  
    score.match[V(gr.ss)$name]

  #sub chebi id's for names
  if (!is.na(annot[1])){
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