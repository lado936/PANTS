#' Plot network diagram for a pathway
#' 
#' Plot network diagram for a pathway with node size corresponding to significance and color to pathway membership.
#' 
#' @param gr A graph of class \code{igraph} representing the interaction network. \code{is_simple(gr)} must be TRUE.
#' @param ker Kernel matrix, can be sparse matrix from package \code{Matrix}.
#' @param Gmat Pathway membership matrix, can be sparse matrix from package \code{Matrix}.
#' @param pwy Pathway to plot. Must be a column name of \code{Gmat}.
#' @param score.v Named vector of scores of features, where \code{names(score.v) == rownames(gr)}, to select top nodes 
#' and color them.
#' @param annot Named vector of annotations for nodes. If \code{annot} is given, \code{names(annot)} should 
#' have some overlap with \code{rownames(Gmat)}
#' @param ntop Number of top most significant features to include. If one of these is an external node, then its
#' internal neighbor nodes are also included. These nodes are then connected based on the interaction network.
#' @param alternative A character string specifying the alternative hypothesis.
#' @param name Name for PDF file to plot. Extension ".pdf" is added to the name. Must be a valid fileneme. Set to 
#' \code{NA} to suppress writing to file.
#' @param color.pal A color palette, as a vector. Must be accepted by \code{\link[igraph]{plot.igraph}}. If \code{NULL},
#' a palette from \code{\link[RColorBrewer]{brewer.pal}} appropriate to \code{alternative} is chosen.
#' @param seed Seed to set using \code{set.seed} for reproducibility of the graph layout.
#' @return Invisibly, a list with components: 
#'  \describe{
#'    \item{gr}{the graph that gets plotted}
#'    \item{vertex.color}{the vertex colors}
#'    \item{vertex.size}{the vertex sizes}
#'    \item{score}{scores of the vertices of the plotted graph}
#' }
#' @export

plot_pwy <- function(gr, ker, Gmat, pwy, score.v, annot=NA, ntop=7, alternative=c("two.sided", "less", "greater"), 
                     name=paste0(gsub(":|/", "_", pwy), '_ntop', ntop), color.pal=NULL, seed=0){
  
  stopifnot(pwy %in% colnames(Gmat), igraph::is_simple(gr))
  if (!is.na(annot) && length(intersect(names(annot), rownames(Gmat))) == 0){
    stop("'annot' must be NA or 'names(annot)' must overlap with 'rownames(Gmat)'.")
  }
  
  alternative <- match.arg(alternative)
  if (is.null(color.pal)){
    if (!requireNamespace("RColorBrewer", quietly = TRUE)){
      stop("Package 'RColorBrewer' needed since 'is.null(color.pal)'. Please install it.", call. = FALSE)
    }
  }
  
  in.shape <- "circle"
  out.shape <- "square"
  
  if (alternative=="two.sided"){
    lim <- c(-max(abs(score.v)), max(abs(score.v)))
    #use yellow in middle to distinguish NA's, which are grey
    if (is.null(color.pal)) color.pal <- RColorBrewer::brewer.pal(n=9, name='RdYlBu')[9:1]
  } else {
    lim <- range(score.v)
    if (is.null(color.pal)) color.pal <- RColorBrewer::brewer.pal(n=9, name='Reds')
  }
  
  sc.m <- as.matrix(data.frame(score.v))
  rownames(sc.m) <- names(score.v)
  mm <- match_mats(score.mat=sc.m, ker=ker, Gmat=Gmat, score.impute = NA)
  score.v <- mm$score.mat[,1,drop=FALSE]; ker <- mm$ker; Gmat <- mm$Gmat
  rm(mm) #to save memory
  #expand graph to include all features, even if they're isolated
  new.v <- setdiff(rownames(Gmat), igraph::V(gr)$name)
  gr <- igraph::add_vertices(graph=gr, nv=length(new.v), name=new.v)
  
  pwy.nodes <- rownames(Gmat)[Gmat[,pwy]>0]
  pwy.neighbors <- setdiff(neighbor_nms(gr, pwy.nodes), pwy.nodes)

  #get kernel weight sums per node
  coeff.sc <- (ker %*% Gmat[,pwy])[,1]
  
  #estimate impact of nodes on pwy score
  #this includes estimation of impact.v to NA nodes, which are properly handled by order() below
  impact.v <- stats::setNames((coeff.sc * score.v)[,1], nm=rownames(score.v))

  #get top n nodes by abs(impact.v)
  top.nodes <- names(impact.v)[switch(alternative, greater=order(-impact.v),
                      less=order(impact.v), two.sided=order(-abs(impact.v)))][1:ntop]
                      
  #get neighbors
  pwy.neighbors.ss <- intersect(pwy.neighbors, top.nodes)
  #get pwy nodes with large stats OR pwy nodes connected to neighbors with large stats
  pwy.nodes.ss <- union(intersect(pwy.nodes, top.nodes),
                        intersect(pwy.nodes, neighbor_nms(gr, pwy.neighbors.ss)))

  gr.pwy <- igraph::induced_subgraph(gr, vid=which(igraph::V(gr)$name %in% c(pwy.nodes.ss, pwy.neighbors.ss)))
  x <- score.v[igraph::V(gr.pwy)$name, 1]
  color.v <- stats::setNames(map2color(x=x, pal=color.pal, limits=lim), nm=names(x))
  shape.v <- c(out.shape, in.shape)[(igraph::V(gr.pwy)$name %in% pwy.nodes)+1]
  names(shape.v) <- igraph::V(gr.pwy)$name

  #sub chebi id's for names
  if (!is.na(annot[1])){
    nms.int <- intersect(names(annot), igraph::V(gr.pwy)$name)
    if (length(nms.int) > 0){
      igraph::V(gr.pwy)$name[match(nms.int, igraph::V(gr.pwy)$name)] <- annot[nms.int]
    }
  }#end if

  #need to gsub disallowed characters
  if (!is.na(name)) grDevices::pdf(paste0(name, '.pdf'))
  set.seed(seed)
  graphics::plot(gr.pwy, vertex.color=color.v, vertex.shape=shape.v)
  legend_colorbar(col=color.pal, lev=lim)
  if (any(shape.v==out.shape)) graphics::legend(x="topright", legend=c("Inside pwy", "Outside pwy"), pch=1:0, bty="n")
  if (!is.na(name)) grDevices::dev.off()

  ret <- list(gr=gr.pwy, vertex.color=color.v, vertex.shape=shape.v, score=x)
  return(invisible(ret))
}