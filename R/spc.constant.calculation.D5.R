spc.constant.calculation.D5 <- function(sample.size, n.sigma = 3, negative.as.NA = T) {
  ret <- 1 - n.sigma * (3.07/3) * spc.constant.calculation.d3(sample.size) / spc.constant.calculation.d4(sample.size)
  
  if (ret < 0 & negative.as.NA) {
    ret <- NA
  }
  
  ret
}
