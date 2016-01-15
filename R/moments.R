moment.k<-function(x, 
                   k = 2, 
                   center = mean,
                   denominator = c("n", "n-1")
) {
  n  <- length(na.omit(x))
  mp <- center(x)
  sum( (x-mp)^k / (eval(parse(text = denominator[1])))   )
}

.moment.m2 <- function(x) {
  moment.k(x,2)
}

.moment.m3 <- function(x) {
  moment.k(x,3)
}

.moment.m4 <- function(x) {
  moment.k(x,4)
}

skewness <- function(x) {
  n  <- length(na.omit(x))
  m2 <- .moment.m2(x)
  m3 <- .moment.m3(x)

  (m3 / m2^(3/2)) * sqrt(n*(n-1))/(n-2)
}

kurtosis <- function(x) {
  n  <- length(na.omit(x))
  m2 <- .moment.m2(x)
  m4 <- .moment.m4(x)
  
  ((n-1)*(n+1)/((n-2)*(n-3)))*m4/m2^2 - 3*(n-1)^2/((n-2)*(n-3))
}


#Test case
#moment.k(c(1,2,2,3,3,3),2)
#var(c(1,2,2,3,3,3))*5/6
