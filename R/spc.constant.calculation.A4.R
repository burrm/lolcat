spc.constant.calculation.A4 <- function(sample.size, n.sigma = 3) {
  sapply(sample.size, FUN = function(i) {
      n.sigma/(spc.constant.calculation.d4(i) * sqrt(i))
  })
}
