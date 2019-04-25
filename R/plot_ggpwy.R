#' Plot network diagram for a pathway
#' 
#' Plot nodes most impacting a pathway as a network diagram with node color corresponding to z-score and 
#' shape to pathway membership. The most impactful nodes are inferred by assuming the input here is the same as
#' was used to calculate pathway significance in \code{\link{pants}} or \code{\link{pants_hitman}}.
#' 
#' @param zscore.v Named vector of feature z-scores to select high impact nodes. These should be finite unless they 
#' are \code{NA}. These are used to define color scale, for consistency across pathway visualizations.
#' @param pwy Pathway, must be a column name of \code{Gmat}.
#' @param annot.v Named vector of annotations for nodes. If \code{annot.v} is given, \code{names(annot.v)} should 
#' have some overlap with \code{rownames(Gmat)}.
#' @param name Name of file to plot to. If \code{NULL}, creates a filename from \code{pwy} and \code{ntop}. Set to 
#' \code{NA} to plot to screen instead of to file.
#' @param plot Logical; should plot be generated?
#' @param signif.dig Number of significant digits to use for colorbar axis labels.
#' @inheritParams graph2kernel
#' @inheritParams pants
#' @inheritParams ezlimma::roast_contrasts
#' @details If some \code{ntop} nodes are outside \code{pwy}, then their neighbor nodes inside \code{pwy} are also 
#' plotted. These nodes are then connected based on the interaction network. Nodes inside the network are drawn as
#' circles, whereas those outside the network are drawn as squares, which is indicated by a legend when some nodes
#' are outside the pathway.
#' 
#' Unmeasured nodes have \code{is.na(zscore.v)} and are drawn as white, which is indicated in the colorbar.
#' 
#' The color palette is from \code{\link[RColorBrewer]{brewer.pal}}, and depends on the \code{alternative}.
#' 
#' @return Invisibly, a list with components: 
#'  \describe{
#'    \item{\code{gr}}{graph that gets plotted}
#'    \item{\code{vertex.color}}{vertex colors}
#'    \item{\code{vertex.shape}}{vertex shapes}
#'    \item{\code{vertex.zscore}}{z-scores of the vertices of the plotted graph}
#'    \item{\code{vertex.impact}}{vertex impact on pathway}
#'    \item{\code{top.nodes}}{ntop top nodes driving pathway}
#' }
#' @export

plot_ggpwy <- function(gr, zscore.v, ntop = 7, Gmat, pwy, ker=NULL, annot.v = NA, alternative = c("two.sided", "less", "greater"), 
                     name = NULL, plot = TRUE, signif.dig=2, seed = 0){
  if (is.null(ker)){
    zscore.mat <- matrix(zscore.v, nrow=length(zscore.v), ncol=1, dimnames=list(names(zscore.v), "scores"))
    ker <- diag_kernel(object=zscore.mat, Gmat=Gmat)
  }
  stopifnot(pwy %in% colnames(Gmat), igraph::is_simple(gr), is.logical(plot), is.na(zscore.v) | is.finite(zscore.v), 
            !is.null(zscore.v))
  if (!is.na(annot.v) && length(intersect(names(annot.v), rownames(Gmat))) == 0) {
      stop("'annot.v' must be NA or 'names(annot.v)' must overlap with 'rownames(Gmat)'.")
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)){
    stop("Package 'RColorBrewer' needed since 'color.pal' is NULL. Please install it.", call. = FALSE)
  }
  alternative <- match.arg(alternative)
  if (is.null(name)) name <- paste0(ezlimma::clean_filenames(pwy), "_ntop", ntop)
  
  # lim doesn't need to worry about zscore.v having NAs, since it doesn't here
  # creates common lim using all zscore.v, so pwys have consistent colorbar
  if (alternative=="two.sided"){
    lim <- c(-max(abs(zscore.v)), max(abs(zscore.v)))
    #use yellow in middle to distinguish NAs, which are grey
    color.pal <- RColorBrewer::brewer.pal(n=9, name="RdYlBu")[9:1]
  } else {
    lim <- range(zscore.v)
    color.pal <- RColorBrewer::brewer.pal(n=9, name="Reds")
  }
  
  # match mats
  zsc.m <- as.matrix(data.frame(zscore.v))
  mm <- match_mats(score.mat=zsc.m, ker=ker, Gmat=Gmat, score.impute = NA)
  zscore.v <- stats::setNames(mm$score.mat[,1], nm=rownames(mm$score.mat))
  ker <- mm$ker; Gmat <- mm$Gmat
  rm(mm) #to save memory
  
  # expand graph to include all features, even if they are isolated
  new.v <- setdiff(rownames(Gmat), igraph::V(gr)$name)
  gr <- igraph::add_vertices(graph=gr, nv=length(new.v), name=new.v)
  
  pwy.nodes <- rownames(Gmat)[Gmat[,pwy]>0]
  pwy.neighbors <- setdiff(neighbor_nms(gr, pwy.nodes), pwy.nodes)

  top.nodes <- select_ntop(zscore.v=zscore.v, Gmat=Gmat, pwy=pwy, ker=ker, alternative=alternative, ntop=ntop)

  # get neighbors
  pwy.neighbors.ss <- intersect(pwy.neighbors, top.nodes$node)
  # get pwy nodes with large stats OR pwy nodes connected to neighbors with large stats
  pwy.nodes.ss <- union(intersect(pwy.nodes, top.nodes$node),
                        intersect(pwy.nodes, neighbor_nms(gr, pwy.neighbors.ss)))

  gr.pwy <- igraph::induced_subgraph(gr, vid=which(igraph::V(gr)$name %in% c(pwy.nodes.ss, pwy.neighbors.ss)))
  igraph::V(gr.pwy)$z <- zscore.v[igraph::V(gr.pwy)$name]
  igraph::V(gr.pwy)$pathway <- c("outside", "inside")[(igraph::V(gr.pwy)$name %in% pwy.nodes)+1]

  # sub annot for names
  # V(gr.pwy)$name already exists
  if (any(!is.na(annot.v))){
    annot.v <- annot.v[!is.na(annot.v)]
    nms.int <- intersect(names(annot.v), igraph::V(gr.pwy)$name)
    if (length(nms.int) > 0){
      igraph::V(gr.pwy)$name[match(nms.int, igraph::V(gr.pwy)$name)] <- annot.v[nms.int]
    }
  }
  
  gg.pwy <- tidygraph::as_tbl_graph(gr.pwy)
  if (plot){
    if (!is.na(name)) grDevices::pdf(paste0(name, ".pdf"))
    set.seed(seed)
    ggg <- ggraph::ggraph(gg.pwy, layout = "nicely") + ggraph::geom_edge_link() + ggraph::theme_graph() + 
      ggraph::geom_node_point(mapping=ggplot2::aes(shape=pathway, color=z), size=6) + 
      ggraph::geom_node_text(mapping=ggplot2::aes(label=I(name))) +
      ggplot2::scale_colour_gradientn(colors = color.pal, limits=lim)
    graphics::plot(ggg)
    if (!is.na(name)) grDevices::dev.off()
  }
  
  # ret <- list(gr=gr.pwy, vertex.color=color.v, vertex.shape=shape.v, vertex.zscore=zscore.ss, 
  #             vertex.impact=setNames(top.nodes[top.nodes$node, "impact"], nm=top.nodes$node), top.node.nms=top.nodes$node)
  return(invisible(gg.pwy))
}