# Integral of 1- F(x)^n - (1-F(x))^n
spc.constant.calculation.d2 <- function(sample.size) {
  d2.f <- function(x) {
    1- pnorm(x)^sample.size - (1-pnorm(x))^sample.size
  }
  
  integrate(d2.f, -Inf, Inf)$value
}


