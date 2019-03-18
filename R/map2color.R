#' Map values to colors
#'
#' Map values to colors in a palette. 
#'
#' @param xx Values to map.
#' @param pal Color palette.
#' @param limits A vector of size 2.
#' @details From \href{https://stackoverflow.com/questions/15006211/how-do-i-generate-a-mapping-from-numbers-to-colors-in-r}{stackoverflow}.

map2color <- function(xx, pal, limits=NULL){
  if (is.null(limits)) limits <- range(xx, na.rm = TRUE)
  stopifnot(length(limits) >= 2)
  pal[findInterval(xx, seq(from=limits[1], to=limits[2], length.out=length(pal)+1), all.inside=TRUE)]
}