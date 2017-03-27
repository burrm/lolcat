spc.constant.calculation.A1 <- function(sample.size, n.sigma = 3) {
  n.sigma/(spc.constant.calculation.c2(sample.size) * sqrt(sample.size))
}
