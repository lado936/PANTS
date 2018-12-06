#' Map values to colors
#'
#' Map values to colors in a palette. 
#'
#' @param x Values to map.
#' @param pal Color palette.
#' @param limits A vector of size 2.
#' @details From \href{https://stackoverflow.com/questions/15006211/how-do-i-generate-a-mapping-from-numbers-to-colors-in-r}{stack overflow}.

map2color <- function(x, pal, limits=NULL){
  if (is.null(limits)) limits <- range(x)
  stopifnot(length(limits) >=2)
  
  pal[findInterval(x, seq(from=limits[1], to=limits[2], length.out=length(pal)+1), all.inside=TRUE)]
}