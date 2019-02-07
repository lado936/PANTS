#' Plot network diagram for a pathway
#' 
#' Plot driver nodes for pathway \code{pwy} as a network diagram with node color corresponding to significance and 
#' shape to pathway membership. The driver nodes are inferred by assuming the input here is the same as was used 
#' to calculate pathway significance in \code{\link{pants}} or \code{\link{pants_hitman}}.
#' 
#' @param score.v Named vector of scores of features to select driver nodes.
#' @param ntop Number of top features that most impact the pathway to include.
#' @param pwy Pathway, must be a column name of \code{Gmat}.
#' @param annot.v Named vector of annotations for nodes. If \code{annot.v} is given, \code{names(annot.v)} should 
#' have some overlap with \code{rownames(Gmat)}.
#' @param name Name of file to create. Set to \code{NA} to plot to screen instead of to file.
#' @param color.pal A color palette, as a vector. Must be accepted by \code{\link[igraph]{plot.igraph}}. If \code{NULL},
#' a palette from \code{\link[RColorBrewer]{brewer.pal}} appropriate to \code{alternative} is chosen.
#' @param plot Logical, should plot be generated?
#' @inheritParams graph2kernel
#' @inheritParams pants
#' @inheritParams ezlimma::roast_contrasts
#' @details It's checked that \code{names(score.v) == rownames(gr)}. If some \code{ntop} nodes are external, then their 
#' internal neighbor nodes are also included. These nodes are then connected based on the interaction network.
#' @return Invisibly, a list with components: 
#'  \describe{
#'    \item{gr}{the graph that gets plotted}
#'    \item{vertex.color}{the vertex colors}
#'    \item{vertex.size}{the vertex sizes}
#'    \item{score}{scores of the vertices of the plotted graph}
#'    \item{top.nodes}{ntop top nodes driving pathway}
#' }
#' @export

plot_pwy <- function(gr, score.v, ntop = 7, Gmat, pwy, ker=NULL, annot.v = NA, alternative = c("two.sided", "less", "greater"), 
                     name = NULL, color.pal = NULL, plot = TRUE, seed = 0){
  
  if (is.null(ker)){
    score.mat <- matrix(score.v, nrow=length(score.v), ncol=1, dimnames=list(names(score.v), "scores"))
    ker <- diag_kernel(object=score.mat, Gmat=Gmat)
  }
  
  stopifnot(pwy %in% colnames(Gmat), igraph::is_simple(gr), is.logical(plot), is.finite(score.v))
  
  if (!is.na(annot.v) && length(intersect(names(annot.v), rownames(Gmat))) == 0) {
      stop("'annot.v' must be NA or 'names(annot.v)' must overlap with 'rownames(Gmat)'.")
  }
  
  alternative <- match.arg(alternative)
  if (is.null(color.pal)){
    if (!requireNamespace("RColorBrewer", quietly = TRUE)){
      stop("Package 'RColorBrewer' needed since 'color.pal' is NULL. Please install it.", call. = FALSE)
    }
  }

  if (is.null(name)) name <- paste0(ezlimma::clean_filenames(pwy), '_ntop', ntop)
  
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
  score.v <- stats::setNames(mm$score.mat[,1], nm=rownames(mm$score.mat)) 
  ker <- mm$ker; Gmat <- mm$Gmat
  rm(mm) #to save memory
  
  #expand graph to include all features, even if they're isolated
  new.v <- setdiff(rownames(Gmat), igraph::V(gr)$name)
  gr <- igraph::add_vertices(graph=gr, nv=length(new.v), name=new.v)
  
  pwy.nodes <- rownames(Gmat)[Gmat[,pwy]>0]
  pwy.neighbors <- setdiff(neighbor_nms(gr, pwy.nodes), pwy.nodes)

  top.nodes <- select_ntop(score.v=score.v, Gmat=Gmat, pwy=pwy, ker=ker, alternative=alternative, ntop=ntop)

  #get neighbors
  pwy.neighbors.ss <- intersect(pwy.neighbors, top.nodes)
  #get pwy nodes with large stats OR pwy nodes connected to neighbors with large stats
  pwy.nodes.ss <- union(intersect(pwy.nodes, top.nodes),
                        intersect(pwy.nodes, neighbor_nms(gr, pwy.neighbors.ss)))

  gr.pwy <- igraph::induced_subgraph(gr, vid=which(igraph::V(gr)$name %in% c(pwy.nodes.ss, pwy.neighbors.ss)))
  x <- score.v[igraph::V(gr.pwy)$name]
  color.v <- stats::setNames(map2color(xx=x, pal=color.pal, limits=lim), nm=names(x))
  shape.v <- c(out.shape, in.shape)[(igraph::V(gr.pwy)$name %in% pwy.nodes)+1]
  names(shape.v) <- igraph::V(gr.pwy)$name

  #sub annot for names
  if (!is.na(annot.v[1])){
    nms.int <- intersect(names(annot.v), igraph::V(gr.pwy)$name)
    if (length(nms.int) > 0){
      igraph::V(gr.pwy)$name[match(nms.int, igraph::V(gr.pwy)$name)] <- annot.v[nms.int]
    }
  }
  
  if (plot){
    if (!is.na(name)) grDevices::pdf(paste0(name, '.pdf'))
    set.seed(seed)
    graphics::par(mar=c(5.1, 4.1, 4.1, 2.5))
    graphics::plot(gr.pwy, vertex.color=color.v, vertex.shape=shape.v)
    legend_colorbar(col=color.pal, lev=lim)
    if (any(shape.v==out.shape)) graphics::legend(x="topright", legend=c("Inside pwy", "Outside pwy"), pch=1:0, bty="n")
    if (!is.na(name)) grDevices::dev.off()
  }
  
  ret <- list(gr=gr.pwy, vertex.color=color.v, vertex.shape=shape.v, score=x, top.nodes=top.nodes)
  return(invisible(ret))
}