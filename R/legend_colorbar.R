#' Plot colorbar legend
#' 
#' Plot colorbar legend
#' @param col Color palette vector
#' @param lev Levels corresponding to colors
#' @details Modified from \url{https://aurelienmadouasse.wordpress.com/2012/01/13/legend-for-a-continuous-color-scale-in-r/}.

legend_colorbar <- function(col, lev){
  opar <- par
  n <- length(col)
  bx <- par("usr")
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000, bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  xx <- rep(box.cx, each = 2)
  
  par(xpd = TRUE)
  #plot colored polygons
  for(i in 1:n){
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
  }
  #add axis
  par(new = TRUE)
  plot(x=0, y=0, type = "n", ylim = c(min(lev), max(lev)), yaxt = "n", ylab = "", xaxt = "n", xlab = "", frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = 0)
  par <- opar
}