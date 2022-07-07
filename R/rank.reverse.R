#' Calculate Reverse Ranks  
#' 
#' Calculates ranks where rank 1 is largest value and rank k is smallest value. 
#'
#' @param x Vector - The object to rank
#' @param ... Additional parameters - Additional parameters for rank(...) (ex. na.last or ties.method)
#'
#' @return Rank values for x from largest to smallest 
#' 
#' @seealso \code{\link{rank}}
rank.reverse <- function(x, ...) {
  x <- -1 *x
  rank(x, ...)
}