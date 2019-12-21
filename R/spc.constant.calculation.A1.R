spc.constant.calculation.A1 <- function(sample.size, n.sigma = 3) {
  sapply(sample.size, FUN = function(i) {
      n.sigma/(spc.constant.calculation.c2(i) * sqrt(i))
  })

}
