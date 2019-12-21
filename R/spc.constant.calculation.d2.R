# Integral of 1- F(x)^n - (1-F(x))^n
spc.constant.calculation.d2 <- function(sample.size) {
  sapply(sample.size, FUN = function(i) {
    d2.f <- function(x) {
      1- pnorm(x)^i - (1-pnorm(x))^i
    }
    
    integrate(d2.f, -Inf, Inf)$value
  })
}