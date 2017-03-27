spc.constant.calculation.B4 <- function(sample.size, n.sigma = 3, exact = T) {
  ret <- if (exact) {
    c2 <- spc.constant.calculation.c2(sample.size)
    1 + (n.sigma / c2) * sqrt((sample.size -1) / sample.size - c2^2 ) 
  } else {
    1 + n.sigma / sqrt(2 * (sample.size-1))
  }
  
  ret
}