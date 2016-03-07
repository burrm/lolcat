spc.constant.calculation.D6 <- function(sample.size, n.sigma = 3) {
  ret <- 1 + n.sigma * (3.07/3) * spc.constant.calculation.d3(sample.size) / spc.constant.calculation.d4(sample.size)
  
  ret
}
