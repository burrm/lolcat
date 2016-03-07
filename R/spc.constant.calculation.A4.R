spc.constant.calculation.A4 <- function(sample.size, n.sigma = 3) {
  n.sigma/(spc.constant.calculation.d4(sample.size) * sqrt(sample.size))
}
