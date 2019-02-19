#' Transform analyte-by-pathway matrix into list compatible with Gene Matrix Transposed (GMT) file format
#' 
#' Transform analyte-by-pathway matrix into list compatible with Gene Matrix Transposed (GMT) file format, described at the \href{https://software.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats#GMT:_Gene_Matrix_Transposed_file_format_.28.2A.gmt.29}{Broad Institute}.
#'
#' @param Gmat A numeric Matrix of features (proteins and/or metabolites) as rows and pathways as columns, indicating 
#' pathway membership with positive elements.
#' @return A list of analyte sets. Each element has a set \code{name}, \code{description}, and \code{genes}.

Gmat2gmt <- function(Gmat){
  stopifnot(ncol(Gmat) > 0, nrow(Gmat) > 0, !is.null(colnames(Gmat)), !is.null(rownames(Gmat)))
  
  gmt.lst <- list()
  for (g.col in colnames(Gmat)){
    gmt.lst[[g.col]]$name <- g.col
    #repeat name in description
    gmt.lst[[g.col]]$description <- g.col
    gmt.lst[[g.col]]$genes <- rownames(Gmat)[Gmat[,g.col] > 0]
  }
  return(gmt.lst)
}
