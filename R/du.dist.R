#' U Shaped Distribution 
#' 
#' Calculate a U Shaped Distribution with support from -a to a. This is also the distribution of a harmonic oscillator.
#'
#' @param a Scalar - numeric - support for distribution (-a,a).
#' @param x Scalar - numeric - quantile.
#' @param q Scalar - numeric - quantile.
#' @param p Scalar - numeric - probability.
#' @param n Scalar - integer - number of observations.
#' @param lower.tail Scalar - logical - if true, probabilities are lower tail probabilities.
#'
#' @references 
#' Robinett, R.W. Quantum and classical probability distributions for position and momentum. Am. J. Phys. 63 (9) September 1995. pp 823-832.
#' 
#' @aliases pu.dist qu.dist ru.dist
#'
#' @return du.dist returns density function, pu.dist returns distribution function, qu.dist returns quantiles, and ru.dist returns random value(s).
#'
#' @examples
#' ## Example 1 - plot the distribution function 
#' # x <- seq(-1.9,1.9,.001)
#' # y <- du.dist(x, a=2)
#' # plot(x,y, type="l", col="blue", main="U Shaped Distribution", xlab="quantile", ylab="density)
#' 
du.dist <- function(x, a = 1) {
  1/(pi * (a^2 - x^2))
}

#' @rdname du.dist 
pu.dist <- function(q, a = 1, lower.tail = T) {
  fn <- function(x) {
    atan(x/sqrt(a^2 - x^2))/pi
  }
  
  if (lower.tail) {
    fn(q)-fn(-a)
  } else {
    fn(a) - fn(q)
  }
}

#' @rdname du.dist 
qu.dist <- function(p, a = 1, lower.tail = T) {
  fn <- function(q) {
    ret <- abs(p - pu.dist(q, a))
    if (!is.finite(ret)) {
      ret <- 999
    }
    ret
  }
  
  zz <- optimize(fn,c(-a,a))
  
  ret <- zz$minimum
  
  if (!lower.tail) {
    ret <- -1*ret
  }
    
  ret
}

#' @rdname du.dist 
ru.dist <- function(n, a = 1) {
  fn <- function(x) {
    qu.dist(x, a=a)
  }
  
  x <- runif(n)
  
  sapply(x, fn)
}


