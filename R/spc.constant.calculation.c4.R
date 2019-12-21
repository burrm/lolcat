spc.constant.calculation.c4 <- function(sample.size) {
  sapply(sample.size, FUN = function(i) {
      spc.constant.calculation.c2(i) * sqrt(i/(i-1))
  })

}