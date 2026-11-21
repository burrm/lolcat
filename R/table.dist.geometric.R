#' Table of Geometric Distribution  
#' 
#' Calculates probabilities for the geometric distribution and includes cumulative probabilities from each tail.
#'
#' @param max.x Scalar - maximum value of x to include (this is an infinite distribution) 
#' @param p Scalar - probability of "success"
#'
#' @return A data frame with x, probability at x, and cumulative probability from each tail to x. 
table.dist.geometric <- function(
  p
  ,max.x = ceiling(4*(1-p)/p^2)
) {
  
  d              <- data.frame(x=0:max.x)
  d$p.at.x       <- dgeom(0:max.x, p = p)
  d$eq.and.above <- pgeom(0:max.x, p = p, lower.tail = F) + d$p.at.x
  d$eq.and.below <- pgeom(0:max.x, p = p)
  
  rownames(d) <- d$x
  
  d
}