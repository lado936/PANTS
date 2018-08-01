#' Calculate power of a matrix
#' 
#' Calculate matrix to an integer power.
#' 
#' @param x a square matrix.
#' @param pow positive integer power to raise the matrix to.
#' @return a matrix.
#' @details This is a simple, recursive function to use integer powers when calculating p-step
#' random walk kernels.

#syntax A^2 is equivalent to A*A (https://www.mathworks.com/help/matlab/ref/mpower.html)
mat_pow = function(x, pow){
  #x must be square
  stopifnot(length(pow)==1, pow >= 1, length(dim(x))==2, nrow(x)==ncol(x))
  if (pow %% 1 != 0) stop("pow must be an integer.")
  if (pow==1) return(x)
  if (pow==2) return(x %*% x)
  if (pow > 2) return(x %*% mat_pow(x, pow-1))
}
