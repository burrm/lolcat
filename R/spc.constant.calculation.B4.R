spc.constant.calculation.B4 <- function(sample.size, n.sigma = 3, exact = T) {
  sapply(sample.size, FUN = function(i) {
    ret <- if (exact) {
      c2 <- spc.constant.calculation.c2(i)
      1 + (n.sigma / c2) * sqrt((i -1) / i - c2^2 ) 
    } else {
      1 + n.sigma / sqrt(2 * (i-1))
    }
    
    ret
  })
}