spc.constant.calculation.A3 <- function(sample.size, n.sigma = 3) {
  n.sigma/(spc.constant.calculation.c4(sample.size) * sqrt(sample.size))
}