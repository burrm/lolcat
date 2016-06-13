#Distribution of harmonic oscillator. 
#Robinett, R.W. Quantum and classical probability distributions for position and momentum. Am. J. Phys. 63 (9) September 1995. pp 823-832.

du.dist <- function(x, a = 1) {
  1/(pi * (a^2 - x^2))
}

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


ru.dist <- function(n, a = 1) {
  fn <- function(x) {
    qu.dist(x, a=a)
  }
  
  x <- runif(n)
  
  sapply(x, fn)
}


