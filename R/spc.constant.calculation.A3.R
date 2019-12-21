spc.constant.calculation.A3 <- function(sample.size, n.sigma = 3) {
  sapply(sample.size, FUN = function(i) {
      n.sigma/(spc.constant.calculation.c4(i) * sqrt(i))
  })
}