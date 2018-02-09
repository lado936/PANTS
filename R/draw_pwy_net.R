#'Draw network diagram for a pathway
#'
#'Draw network diagram for a pathway with node size corresponding to significance and color to pathway membership.
#'
#'@param edge.lst Edge list of global network.
#'@param ker Kernel matrix, can be sparse matrix from package \code{Matrix}.
#'@param gmat Pathway membership matrix, can be sparse matrix from package \code{Matrix}.
#'@param pwy Pathway to plot. Must be a column name of \code{gmat}.
#'@param pval.v Namd vector of p-values of features. \code{names(pval.v)} should equal \code{rownames(net)}.
#'@param annot Named vector of annotations for nodes. If \code{annot} is not \code{NA}, \code{names(annot)} should
#'overlap with \code{rownames(gmat)}.
#'@param ntop Number of top most significant features to include. If one of these is an external node, then its
#'internal neighbor nodes are also included. These nodes are then connected based on the network.
#'@param in.col Color for nodes in a pathway.
#'@param out.col Color for nodes outside of a pathway.
#'@param name.pdf Name for PDF file to plot to. Can't contain characters ":" or "/" on Windows. Set to \code{NA}
#'to suppress writing to file.
#'@export

draw_pwy_net <- function(edge.lst, ker, gmat, pwy, pval.v, annot=NA, ntop=7, in.col='lightblue', out.col='red',
                         name.pdf=paste0(gsub(":|/", "_", pwy), '_ntop', ntop)){
  net <- igraph::simplify(graph.data.frame(edge.lst, directed = FALSE))
  if (packageVersion("igraph") < "1.0.0") {
    net.mat <- igraph::get.adjacency(net)
  } else {
    net.mat <- igraph::as_adjacency_matrix(net)
  }
  pwy.nodes <- rownames(gmat)[gmat[,pwy]>0]

  pval.match <- setNames(rep(median(pval.v), nrow(ker)), nm=rownames(ker))
  pval.match[names(pval.v)] <- pval.v

  stopifnot(pwy.nodes %in% rownames(net.mat), names(pval.v)==rownames(net), colnames(ker)==rownames(gmat),
            names(pval.v) %in% rownames(ker), is.na(annot)|length(intersect(names(annot), rownames(gmat))) > 0)

  # pwy.neighbors <- unique(colnames(net.mat)[colSums(net.mat[pwy.nodes,])>0])
  pwy.neighbors <- setdiff(neighbor_nms(net, pwy.nodes), pwy.nodes)

  #get kernel weight sums per node
  ker.v0 <- ker %*% gmat[,pwy]
  #want mean of ker weights to be 1 to standardize threshold, but want to keep weights non-neg,
  #so divide by mean (instead of subtracting mean)
  ker.v <- setNames(as.numeric(ker.v0 / mean(ker.v0[,1])), nm=rownames(gmat))
  #names(ker.v) <- rownames(gmat)

  #limit estimation of impact.v to nodes that have data
  impact.v <- setNames(rep(0, nrow(ker)), nm=rownames(ker))
  impact.v[names(pval.v)] <- ker.v[names(pval.v)] * qnorm(p=pval.v, lower.tail=FALSE)

  #get top n nodes
  top.nodes <- names(impact.v)[order(impact.v, decreasing = TRUE)][1:ntop]

  #get neighbors
  pwy.neighbors.ss <- intersect(pwy.neighbors, top.nodes)
  #get pwy nodes with large stats OR pwy nodes connected to neighbors with large stats
  pwy.nodes.ss <- union(intersect(pwy.nodes, top.nodes),
                        intersect(pwy.nodes, neighbor_nms(net, pwy.neighbors.ss)))
  if (packageVersion("igraph") < "1.0.0"){
    net.ss <- igraph::induced.subgraph(net, vids=which(V(net)$name %in% c(pwy.nodes.ss, pwy.neighbors.ss)))
  } else {
    net.ss <- igraph::induced_subgraph(net, vid=which(V(net)$name %in% c(pwy.nodes.ss, pwy.neighbors.ss)))
  }

  color.v <- c(out.col, in.col)[(V(net.ss)$name %in% pwy.nodes)+1]
  #arbitrary monotonic transformation
  sz.v <- 2+qchisq(p=pval.match[V(net.ss)$name], df=4, lower.tail=FALSE)

  #sub chebi id's for names
  if (!is.na(annot)){
    nms.int <- intersect(names(annot), V(net.ss)$name)
    if (length(nms.int) > 0){
      V(net.ss)$name[match(nms.int, V(net.ss)$name)] <- annot[nms.int]
    }
  }

  #need to gsub disallowed characters
  if (!is.na(name.pdf)) pdf(paste0(name.pdf), '.pdf')
  plot(net.ss, vertex.color=color.v, vertex.size=sz.v)
  if (!is.na(name.pdf)) dev.off()

  return(invisible(TRUE))
}

##check
# sum(is.na(pwy.neighbors))==0
# sum(is.na(pwy.neighbors.ss))==0
