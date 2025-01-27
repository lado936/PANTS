% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_pwy.R
\name{plot_pwy}
\alias{plot_pwy}
\title{Plot network diagram for a pathway}
\usage{
plot_pwy(feat.tab, impact.tab, Gmat.pwy, gr, ntop = 7, name = NULL,
  plot = TRUE, seed = 0)
}
\arguments{
\item{feat.tab}{Data frame  for all analytes with rownames as analyte names, which should
have some overlap with \code{rownames(Gmat.pwy)}.
1st column with stats, and 2nd column with annotation.
Stats should be finite unless they are \code{NA}. Stats for all nodes are used to define color scale,
for consistency across pathway visualizations. 1st column name used as stat descriptor in plot.
Annotation gives node names to replace analyte IDs. Can be all \code{NA}; \code{NA}s are ignored.}

\item{impact.tab}{Data frame with rownames as analyte names and a column named \code{"impact"}
(ignoring upper vs lowercase) with top analytes for this pathway.
This table can read from CSV files written by \code{\link{pants}}.}

\item{Gmat.pwy}{Binary feature by pathway inclusion matrix, with this pathway as 1st column.
Accessible via \code{Gmat.pwy[, pathway_name, drop=FALSE]}.}

\item{gr}{graph object of class \code{igraph}.}

\item{ntop}{Number of top impactful analytes to plot. Their network neighbors may also be included.}

\item{name}{Name of file to plot to. If \code{NULL}, a filename is created using \code{colnames(Gmat.pwy)[1]}.
Set to \code{NA} to plot to screen instead of to file.}

\item{plot}{Logical; should plot be generated?}

\item{seed}{Integer seed to set for reproducility if \code{fun="mroast"}, since \code{mroast} uses random 
simulations. Ignored if \code{fun="fry"}.}
}
\value{
Invisibly, a \code{\link[tidygraph]{tbl_graph}}, a subclass of 
\pkg{igraph} so every \pkg{igraph} method will work as expected.
}
\description{
Plot nodes most impacting a pathway as a network diagram with node color corresponding to z-score and
shape to pathway membership. The most impactful nodes are inferred by assuming the input here is the same as
was used to calculate pathway significance in \code{\link{pants}} with or without mediation.
}
\details{
If some \code{ntop} nodes are outside \code{pwy}, then their neighbor nodes inside \code{pwy} are also
plotted. These nodes are then connected based on the interaction network.

Unmeasured nodes have stat of \code{NA} and are drawn gray.
}
