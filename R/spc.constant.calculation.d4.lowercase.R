#' @rdname spc.constant.calculation.D4

#OK for n <=35, but 3-4 decimal places after n=35
#Long term - implement better numerics for qtukey...
spc.constant.calculation.d4 <- function(
  sample.size
) {
  sapply(sample.size, FUN = function(i) {
    if (i <= 35) {
      qtukey(.5,i,Inf, lower.tail = F)
    } else {
      spc.constant.calculation.d2(i)-.047
    }
  })

}
