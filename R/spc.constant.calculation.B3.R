spc.constant.calculation.B3 <- function(sample.size, n.sigma = 3, negative.as.NA = T, exact = T) {
  sapply(sample.size, FUN = function(i) {
    ret <- if (exact) {
      c2 <- spc.constant.calculation.c2(i)
      1 - (n.sigma / c2) * sqrt((i -1) / i - c2^2 ) 
    } else {
      1- n.sigma / sqrt(2 * (i-1))
    }

    if (ret < 0 & negative.as.NA) {
      ret <- NA
    }
    
    ret
  })

}