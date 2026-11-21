#' Table of Poisson Distribution  
#' 
#' Calculates probabilities for the poisson distribution and includes cumulative probabilities from each tail.
#'
#' @param lambda Scalar - mean of poisson distribution 
#' @param include.lambda Scalar - how many lambdas of x to include in table
#' @param include.x Scalar - maximum value of x to include
#'
#' @return A data frame with x, probability at x, and cumulative probability from each tail to x. 
table.dist.poisson <- function(
  lambda = 5
  ,include.lambda = ifelse(
    abs(1-lambda)<.3,
    max(10,10/lambda),
    max(5, 5/lambda)
  )
  ,include.x = ceiling(include.lambda*lambda)
) {
  n <- include.x
  
  d              <- data.frame(x=0:n)
  d$p.at.x       <- dpois(0:n, lambda = lambda)
  d$eq.and.above <- ppois(0:n, lambda = lambda, lower.tail = F) + d$p.at.x
  d$eq.and.below <- ppois(0:n, lambda = lambda)
  
  rownames(d) <- d$x
  
  d
  
  
}