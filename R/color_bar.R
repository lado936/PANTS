#' Plot colorbar
#' 
#' Plot colorbar. There are many functions for this in other pacakges, but they do not appear to handle NAs.
#' 
#' @param cols Color palette vector. First value can be NA. \code{NA} is transformed to white, so \code{cols} cannot have
#' \code{"white"} or its hex version \code{"#ffffff"}.
#' @param lev Vector of numeric levels corresponding to colors. The min and max of this vector is used to define numeric 
#' range. Cannot have NAs.
#' @inheritParams plot_pwy
#' @details Modified from \url{https://aurelienmadouasse.wordpress.com/2012/01/13/legend-for-a-continuous-color-scale-in-r/}.
#' @return Invibly, data frame with axis labels and positions.

#an alternative is imageplot() from {fields}, which includes legends, but {fields} has many dependencies
color_bar <- function(cols, lev, signif.dig=2, plot=TRUE){
  whites <- c("white", "#ffffff")
  stopifnot(!is.na(cols[-1]), !(whites %in% cols), !is.na(lev))
  opar <- par
  #usr: c(x1, x2, y1, y2) = user coordinates of the plotting region; maybe bx = box
  bx <- stats::setNames(graphics::par("usr"), nm=c("x1", "x2", "y1", "y2"))
  #box.cx = c(x1, x2), where these are to right of plotting region
  # box.cx <- c(x1 = bx["x2"] + (bx["x2"] - bx["x1"]) / 1000, x2 = bx["x2"] + (bx["x2"] - bx["x1"]) / 1000 + (bx["x2"] - bx["x1"]) / 50)
  box.cx <- c(x1 = bx["x2"], x2 = bx["x2"] + (bx["x2"] - bx["x1"]) / 100)
  #seg.len is y length of colorbar segments
  seg.len <- (bx["y2"] - bx["y1"]) / length(cols)
  #x coordinates of polygon
  poly.x <- rep(box.cx, each = 2)
  if (any(is.na(cols))){
    cols[is.na(cols)] <- "#ffffff" #white
    #extend ylim down by y length of colorbar segments
    lev.min <- min(lev) - seg.len
    ax.labs <- c("NA", signif(seq.int(from=min(lev), to=max(lev), length.out=length(cols)-1), digits=signif.dig))
    # axTicks = seq.int(axp[1L], axp[2L], length.out = 1L + abs(axp[3L]))
    ax.at <- signif(graphics::axTicks(side = 4, axp = c(y1=lev.min, y2=max(lev), n=length(cols)-1)), digits=signif.dig)
  } else {
    lev.min <- min(lev)
    ax.at <- signif(graphics::axTicks(side = 4, axp = c(y1=lev.min, y2=max(lev), n=length(cols) - 1)), digits=signif.dig)
  }
  
  if (plot){
    graphics::par(xpd = TRUE)
    # plot colored polygons
    # yy = c(y1, y2, y2, y1)
    for(col.ind in 1:length(cols)){
      #y coordinates of polygon
      poly.y <- bx["y1"] + c(seg.len*(col.ind-1), seg.len*col.ind, seg.len*col.ind, seg.len*(col.ind-1))
      graphics::polygon(poly.x, poly.y, col = cols[col.ind], border = "black")
    }
    #add axis
    graphics::par(new = TRUE) # plot() should not clean the frame before drawing as if it were on a new device
    # type="n" --> no plotting
    graphics::plot(x=0, y=0, type = "n", ylim = c(lev.min, max(lev)), yaxt = "n", ylab = "", xaxt = "n", xlab = "", frame.plot = FALSE)
    # axis axp = par("yaxp")
    if (any(whites %in% cols)){
      graphics::axis(side = 4, las = 2, tick = FALSE, line = 0, cex.axis=0.7, labels = ax.labs, at = ax.at)
    } else {
      graphics::axis(side = 4, las = 2, tick = FALSE, line = 0, cex.axis=0.7, at = ax.at)
      ax.labs <- ax.at
    }
  }
  par <- opar
  res.df <- data.frame(barcolors=cols, axis.positions=ax.at, stringsAsFactors = FALSE)
  return(invisible(res.df))
}