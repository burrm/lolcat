spc.constant.calculation.B3 <- function(sample.size, n.sigma = 3, negative.as.NA = T, exact = T) {
  ret <- if (exact) {
    c2 <- spc.constant.calculation.c2(sample.size)
    1 - (n.sigma / c2) * sqrt((sample.size -1) / sample.size - c2^2 ) 
  } else {
    1- n.sigma / sqrt(2 * (sample.size-1))
  }

  if (ret < 0 & negative.as.NA) {
    ret <- NA
  }
  
  ret
}