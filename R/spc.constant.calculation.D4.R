spc.constant.calculation.D4 <- function(sample.size, n.sigma = 3) {
  ret <- 1 + n.sigma * spc.constant.calculation.d3(sample.size) / spc.constant.calculation.d2(sample.size)
  
  ret
}

#OK for n <=35, but 3-4 decimal places after n=35
#Long term - implement better numerics for qtukey...
spc.constant.calculation.d4 <- function(sample.size) {
  if (sample.size <= 35) {
    qtukey(.5,sample.size,Inf, lower.tail = F)
  } else {
    spc.constant.calculation.d2(sample.size)-.047
  }
}
