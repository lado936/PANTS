#' Plot network diagram for a pathway
#'
#' Plot nodes most impacting a pathway as a network diagram with node color corresponding to z-score and
#' shape to pathway membership. The most impactful nodes are inferred by assuming the input here is the same as
#' was used to calculate pathway significance in \code{\link{pants}} with or without mediation.
#'
#' @param feat.tab Data frame  for all analytes with rownames as analyte names, which should
#' have some overlap with \code{rownames(Gmat.pwy)}.
#' 1st column with stats, and 2nd column with annotation.
#' Stats should be finite unless they are \code{NA}. Stats for all nodes are used to define color scale,
#' for consistency across pathway visualizations. 1st column name used as stat descriptor in plot.
#' Annotation gives node names to replace analyte IDs. Can be all \code{NA}; \code{NA}s are ignored.
#' @param impact.tab Data frame with rownames as analyte names and a column named \code{"impact"}
#' (ignoring upper vs lowercase) with top analytes for this pathway.
#' This table can read from CSV files written by \code{\link{pants}}.
#' @param Gmat.pwy Binary feature by pathway inclusion matrix, with this pathway as 1st column.
#' Accessible via \code{Gmat.pwy[, pathway_name, drop=FALSE]}.
#' @param ntop Number of top impactful analytes to plot. Their network neighbors may also be included.
#' @param name Name of file to plot to. If \code{NULL}, a filename is created using \code{colnames(Gmat.pwy)[1]}.
#' Set to \code{NA} to plot to screen instead of to file.
#' @param plot Logical; should plot be generated?
#' @inheritParams graph2kernel
#' @inheritParams ezlimma::roast_contrasts
#' @details If some \code{ntop} nodes are outside \code{pwy}, then their neighbor nodes inside \code{pwy} are also
#' plotted. These nodes are then connected based on the interaction network.
#'
#' Unmeasured nodes have stat of \code{NA} and are drawn gray.
#'
#' @return Invisibly, a \code{\link[tidygraph]{tbl_graph}}, a subclass of 
#' \pkg{igraph} so every \pkg{igraph} method will work as expected.
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @export

# alternative?
# do not calculate redundantly to p(h)
# check if data 2-sided instead of alternative, which may be diff than used in p(h), so confusing
# cannot infer graph from ker
plot_pwy <- function(feat.tab, impact.tab, Gmat.pwy, gr, ntop = 7, name = NULL, plot = TRUE, seed = 0){
  stopifnot(ncol(feat.tab)>=2, limma::isNumeric(feat.tab[,1]), !is.null(colnames(feat.tab)),
            !is.null(rownames(feat.tab)), all(is.na(feat.tab[,1]) | is.finite(feat.tab[,1])),
            !is.null(colnames(impact.tab)), !is.null(colnames(Gmat.pwy)), igraph::is_simple(gr), is.numeric(ntop),
            is.logical(plot), is.numeric(seed))
  stat.nm <- colnames(feat.tab)[1]
  if (any(!is.na(feat.tab[,2])) && length(intersect(rownames(feat.tab), rownames(Gmat.pwy))) == 0) {
      stop("'feat.tab[,2]' must be NA or 'rownames(feat.tab[,2])' must overlap with 'rownames(Gmat.pwy)'.")
  }
  impact.colnm <- grep("impact", colnames(impact.tab), value = TRUE, ignore.case = TRUE)
  if (length(impact.colnm)==0) stop("'impact.tab' has not column named 'impact'.") else impact.colnm <- impact.colnm[1]
  pwy.nm <- colnames(Gmat.pwy)[1]
  if (ntop > nrow(impact.tab)) ntop <- nrow(impact.tab)
  if (is.null(name)) name <- paste0(ezlimma::clean_filenames(pwy.nm), "_ntop", ntop)

  # expand graph to include all features, even if they are isolated
  new.v <- setdiff(rownames(Gmat.pwy), igraph::V(gr)$name)
  gr <- igraph::add_vertices(graph=gr, nv=length(new.v), name=new.v)

  pwy.nodes <- rownames(Gmat.pwy)[Gmat.pwy[,pwy.nm]>0]
  pwy.neighbors <- setdiff(neighbor_nms(gr, pwy.nodes), pwy.nodes)

  top.nodes <- rownames(impact.tab)[order(-abs(impact.tab[,impact.colnm]))][1:ntop]

  # get neighbors
  pwy.neighbors.ss <- intersect(pwy.neighbors, top.nodes)
  # get pwy nodes with large stats OR pwy nodes connected to neighbors with large stats
  pwy.nodes.ss <- union(intersect(pwy.nodes, top.nodes),
                        intersect(pwy.nodes, neighbor_nms(gr, pwy.neighbors.ss)))

  gr.pwy <- igraph::induced_subgraph(gr, vid=which(igraph::V(gr)$name %in% c(pwy.nodes.ss, pwy.neighbors.ss)))

  # sub annot for names
  # V(gr.pwy)$name already exists
  if (any(!is.na(feat.tab[,2]))){
    feat.tab[,2] <- feat.tab[,2][!is.na(feat.tab[,2])]
    nms.int <- intersect(names(feat.tab[,2]), igraph::V(gr.pwy)$name)
    if (length(nms.int) > 0){
      igraph::V(gr.pwy)$name[match(nms.int, igraph::V(gr.pwy)$name)] <- feat.tab[,2][nms.int]
    }
  }

  gg.pwy <- tidygraph::as_tbl_graph(gr.pwy) %>% 
    dplyr::mutate(Pathway = c("outside", "inside")[(igraph::V(gr.pwy)$name %in% pwy.nodes)+1])  %>% 
    dplyr::mutate(!!stat.nm := feat.tab[igraph::V(gr.pwy)$name, 1])
  
  if (plot){
    if (!is.na(name)) grDevices::pdf(paste0(name, ".pdf"))
    try({
      set.seed(seed)
      # need to set font fam to avoid "font fam not found" errors
      # geom_label() draws a rectangle behind the text
      # repel repels labels from node centers?
      ggg <- ggraph::ggraph(gg.pwy, layout = "nicely") + ggraph::geom_edge_link() +
        ggraph::theme_graph(base_family = "sans") + 
        ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size=4))) +
        ggraph::geom_node_point(mapping=ggplot2::aes(shape=Pathway, color = !!rlang::ensym(stat.nm)), size=12) + 
        ggraph::geom_node_text(mapping=ggplot2::aes(label=I(name)), repel = FALSE)
      
      # creates common lim using all feat.tab[,1], so pwys have consistent colorbar
      if (min(feat.tab[,1])<0 && max(feat.tab[,1])>0){
        # use yellow in middle to distinguish NAs, which are grey
        ggg <- ggg + ggplot2::scale_colour_distiller(type="div", palette = "RdYlBu", direction = -1,
                                                     limits=c(-max(abs(feat.tab[,1])), max(abs(feat.tab[,1]))))
      } else {
        ggg <- ggg + ggplot2::scale_colour_distiller(type="seq", palette = "Reds", direction = 1, 
                                                   limits=range(feat.tab[,1]))
      }
      graphics::plot(ggg)
    })
    if (!is.na(name)) grDevices::dev.off()
  }
  return(invisible(gg.pwy))
}
