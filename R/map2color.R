#' Map values to colors
#'
#' Map values to colors in a palette. 
#'
#' @param x Values to map.
#' @param pal Color palette.
#' @param limits A vector of size 2.
#' @details From \href{https://stackoverflow.com/questions/15006211/how-do-i-generate-a-mapping-from-numbers-to-colors-in-r}{stack overflow}.

map2color <- function(x, pal, limits = NULL) {
    if (is.null(limits)) 
        limits <- range(x)
    pal[findInterval(x, seq(limits[1], limits[2], length.out = length(pal) + 1), all.inside = TRUE)]
}
