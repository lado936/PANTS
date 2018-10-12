#' Plot colorbar legend
#' 
#' Plot colorbar legend
#' @param col Color palette vector
#' @param lev Vector of levels corresponding to colors. The min and max of this vector is used to define numeric range.
#' @details Modified from \url{https://aurelienmadouasse.wordpress.com/2012/01/13/legend-for-a-continuous-color-scale-in-r/}.

#an alternative is imageplot() from {fields}, which includes legends, but {fields} has many dependencies
legend_colorbar <- function(col, lev){
  opar <- par
  #usr: c(x1, x2, y1, y2) = user coordinates of the plotting region; maybe bx = box
  bx <- setNames(graphics::par("usr"), nm=c("x1", "x2", "y1", "y2"))
  #box.cx = c(x1, x2), where these are to right of plotting region
  # box.cx <- c(x1 = bx["x2"] + (bx["x2"] - bx["x1"]) / 1000, x2 = bx["x2"] + (bx["x2"] - bx["x1"]) / 1000 + (bx["x2"] - bx["x1"]) / 50)
  box.cx <- c(x1 = bx["x2"], x2 = bx["x2"] + (bx["x2"] - bx["x1"]) / 100)
  #seg.len is y length of colorbar segments
  seg.len <- (bx["y2"] - bx["y1"]) / length(col)
  #x coordinates of polygon
  poly.x <- rep(box.cx, each = 2)
  
  graphics::par(xpd = TRUE)
  #plot colored polygons
  #yy = c(y1, y2, y2, y1)
  for(i in 1:length(col)){
    #y coordinates of polygon
    poly.y <- c(bx["y1"] + (seg.len * (i - 1)),
            bx["y1"] + (seg.len * (i)),
            bx["y1"] + (seg.len * (i)),
            bx["y1"] + (seg.len * (i - 1)))
    graphics::polygon(poly.x, poly.y, col = col[i], border = col[i])
  }
  #add axis
  graphics::par(new = TRUE)
  graphics::plot(x=0, y=0, type = "n", ylim = c(min(lev), max(lev)), yaxt = "n", ylab = "", xaxt = "n", xlab = "", frame.plot = FALSE)
  graphics::axis(side = 4, las = 2, tick = FALSE, line = 0, at = round(axTicks(side = 4, axp = c(min(lev), max(lev), length(col) - 1)), 1),
                 cex.axis=0.7)
  par <- opar
}