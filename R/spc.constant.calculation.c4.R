spc.constant.calculation.c4 <- function(sample.size) {
  spc.constant.calculation.c2(sample.size) * sqrt(sample.size/(sample.size-1))
}