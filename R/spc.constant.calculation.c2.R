spc.constant.calculation.c2 <- function(sample.size) {
  sapply(sample.size, FUN = function(i) {
    sqrt(2/i)*gamma(i/2)/gamma((i-1)/2)
  })

}