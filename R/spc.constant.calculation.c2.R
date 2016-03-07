spc.constant.calculation.c2 <- function(sample.size) {
  sqrt(2/sample.size)*gamma(sample.size/2)/gamma((sample.size-1)/2)
}