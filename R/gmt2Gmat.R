#' Transform list from GMT into feature-by-pathway matrix
#'
#' Transform list from reading in GMT using \code{ezlimma::read_gmt} into feature-by-pathway matrix.
#'
#' @param gmt.lst List returned by \code{\link[ezlimma]{read_gmt}}.
#' @return A numeric Matrix of features (e.g. proteins) as rows and pathways as columns, indicating pathway membership.
#' @export

gmt2Gmat <- function(gmt.lst){
  stopifnot(is.list(gmt.lst))

  all.feats <- sort(unique(unlist(sapply(gmt.lst, FUN=function(x) x[3]))))
  all.pwys <- sort(unique(unlist(sapply(gmt.lst, FUN=function(x) x[1]))))

  Gmat <- Matrix::Matrix(0, nrow=length(all.feats), ncol=length(all.pwys), dimnames = list(all.feats, all.pwys))
  for (i in 1:length(gmt.lst)){
    Gmat[ gmt.lst[[i]][[3]], gmt.lst[[i]][[1]] ] <- 1
  }
  return(Gmat)
}
