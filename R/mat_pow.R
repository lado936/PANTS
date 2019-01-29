#' Calculate power of a matrix
#' 
#' Calculate matrix to an integer power.
#' 
#' @param xx a square matrix.
#' @param pow positive integer power to raise the matrix to.
#' @return a matrix.
#' @details This is a simple, recursive function to use integer powers when calculating p-step
#' random walk kernels.

#syntax A^2 is equivalent to A*A (https://www.mathworks.com/help/matlab/ref/mpower.html)
mat_pow = function(xx, pow){
  #x must be square
  stopifnot(length(pow)==1, pow >= 1, length(dim(xx))==2, nrow(xx)==ncol(xx))
  if (pow %% 1 != 0) stop("pow must be an integer.")
  if (pow==1) return(xx)
  if (pow==2) return(xx %*% xx)
  if (pow > 2) return(xx %*% mat_pow(xx, pow-1))
}
