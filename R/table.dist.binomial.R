#' Table of Binomial Distribution  
#' 
#' Calculates probabilities for the binomial distribution and includes cumulative probabilities from each tail.
#'
#' @param n Scalar - sample size 
#' @param p Scalar - probability of "success"
#'
#' @return A data frame with x, probability at x, and cumulative probability from each tail to x. 
table.dist.binomial <- function(n = 10
                                ,p = .5) {
  
  d              <- data.frame(x=0:n)
  d$p.at.x       <- dbinom(0:n, size = n, prob = p)
  d$eq.and.above <- pbinom(0:n, size=n, prob = p, lower.tail = F) + d$p.at.x
  d$eq.and.below <- pbinom(0:n, size=n, prob = p)
  
  rownames(d) <- d$x
  
  d
}