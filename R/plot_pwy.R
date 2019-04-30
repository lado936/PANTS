#' Plot network diagram for a pathway
#' 
#' Plot nodes most impacting a pathway as a network diagram with node color corresponding to significance and 
#' shape to pathway membership. The most impactful nodes are inferred by assuming the input here is the same as
#' was used to calculate pathway significance in \code{\link{pants}} or \code{\link{pants_hitman}}.
#' 
#' @param zscore.v Named vector of feature z-scores to select high impact nodes. These should be finite unless they 
#' are \code{NA}.
#' @param pwy Pathway, must be a column name of \code{Gmat}.
#' @param annot.v Named vector of annotations for nodes. If \code{annot.v} is given, \code{names(annot.v)} should 
#' have some overlap with \code{rownames(Gmat)}.
#' @param alternative Alternative of interest for analyte z-scores. This may be \code{two.sided}, even though 
#' \code{pants} tested the absolute value of scores. 
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

plot_pwy <- function(gr, zscore.v, ntop = 7, Gmat, pwy, ker=NULL, annot.v = NA, alternative = c("two.sided", "less", "greater"), 
                     name = NULL, plot = TRUE, signif.dig=2, seed = 0){
  if (is.null(ker)){
    zscore.mat <- matrix(zscore.v, nrow=length(zscore.v), ncol=1, dimnames=list(names(zscore.v), "scores"))
    ker <- diag_kernel(object=zscore.mat, Gmat=Gmat)
  }
  stopifnot(pwy %in% colnames(Gmat), igraph::is_simple(gr), is.logical(plot), all(is.na(zscore.v) | is.finite(zscore.v)), 
            !is.null(zscore.v))
  if (!is.na(annot.v) && length(intersect(names(annot.v), rownames(Gmat))) == 0) {
      stop("'annot.v' must be NA or 'names(annot.v)' must overlap with 'rownames(Gmat)'.")
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)){
    stop("Package 'RColorBrewer' needed since 'color.pal' is NULL. Please install it.", call. = FALSE)
  }
  alternative <- match.arg(alternative)
  if (is.null(name)) name <- paste0(ezlimma::clean_filenames(pwy), "_ntop", ntop)
  
  in.shape <- "circle"
  out.shape <- "square"
  
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
  
  zsc.m <- as.matrix(data.frame(zscore.v))
  mm <- match_mats(score.mat=zsc.m, ker=ker, Gmat=Gmat, score.impute = NA)
  zscore.v <- stats::setNames(mm$score.mat[,1], nm=rownames(mm$score.mat)) 
  ker <- mm$ker; Gmat <- mm$Gmat
  rm(mm) #to save memory
  
  #expand graph to include all features, even if they are isolated
  new.v <- setdiff(rownames(Gmat), igraph::V(gr)$name)
  gr <- igraph::add_vertices(graph=gr, nv=length(new.v), name=new.v)
  
  pwy.nodes <- rownames(Gmat)[Gmat[,pwy]>0]
  pwy.neighbors <- setdiff(neighbor_nms(gr, pwy.nodes), pwy.nodes)

  top.nodes <- select_ntop(zscore.v=zscore.v, Gmat=Gmat, pwy=pwy, ker=ker, alternative=alternative, ntop=ntop)
  # top.node.nms <- top.nodes$node

  #get neighbors
  pwy.neighbors.ss <- intersect(pwy.neighbors, top.nodes$node)
  #get pwy nodes with large stats OR pwy nodes connected to neighbors with large stats
  pwy.nodes.ss <- union(intersect(pwy.nodes, top.nodes$node),
                        intersect(pwy.nodes, neighbor_nms(gr, pwy.neighbors.ss)))

  gr.pwy <- igraph::induced_subgraph(gr, vid=which(igraph::V(gr)$name %in% c(pwy.nodes.ss, pwy.neighbors.ss)))
  zscore.ss <- zscore.v[igraph::V(gr.pwy)$name]
  # color.v node colors is NA where xx is NA
  color.v <- stats::setNames(map2color(xx=zscore.ss, pal=color.pal, limits=lim), nm=names(zscore.ss))
  shape.v <- c(out.shape, in.shape)[(igraph::V(gr.pwy)$name %in% pwy.nodes)+1]
  names(shape.v) <- igraph::V(gr.pwy)$name

  # sub annot for names
  if (!is.na(annot.v[1])){
    nms.int <- intersect(names(annot.v), igraph::V(gr.pwy)$name)
    if (length(nms.int) > 0){
      igraph::V(gr.pwy)$name[match(nms.int, igraph::V(gr.pwy)$name)] <- annot.v[nms.int]
    }
  }
  
  if (plot){
    if (!is.na(name)) grDevices::pdf(paste0(name, ".pdf"))
    set.seed(seed)
    graphics::par(mar=c(5.1, 4.1, 4.1, 2.5))
    graphics::plot(gr.pwy, vertex.color=color.v, vertex.shape=shape.v, vertex.label.font=2) #bold label font
    if (any(is.na(zscore.v))){
      color_bar(cols=c(NA, color.pal), lev=lim, signif.dig=signif.dig)
    } else {
      color_bar(cols=color.pal, lev=lim, signif.dig=signif.dig)
    }
    if (any(shape.v==out.shape)) graphics::legend(x="topright", legend=c("Inside pathway", "Outside pathway"), pch=1:0, bty="n")
    if (!is.na(name)) grDevices::dev.off()
  }
  ret <- list(gr=gr.pwy, vertex.color=color.v, vertex.shape=shape.v, vertex.zscore=zscore.ss, 
              vertex.impact=stats::setNames(top.nodes[top.nodes$node, "impact"], nm=top.nodes$node), top.node.nms=top.nodes$node)
  return(invisible(ret))
}