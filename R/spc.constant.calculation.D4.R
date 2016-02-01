.spc.cached.constant.D4 <- data.frame(
  n = 2:15,
  D4 = c(3.267, 2.574, 2.282, 2.115,
         2.004, 1.924, 1.864, 1.816,
         1.777, 1.744, 1.717, 1.693,
         1.672, 1.653)
)

spc.constant.calculation.D4 <- function(n) {
  ret <- NA
  
  idx <- which(.spc.cached.constant.D4$n == n)
  if (length(idx) != 0) {
    ret <- .spc.cached.constant.D4$D4[idx]
  } 
  
  ret
}