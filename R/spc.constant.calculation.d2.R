.spc.cached.constant.d2 <- data.frame(
  n = 2:15,
  d2 = c(1.128, 1.693, 2.059, 2.326,
         2.534, 2.704, 2.847, 2.970,
         3.078, 3.173, 3.258, 3.336,
         3.407, 3.472)
)

spc.constant.calculation.d2 <- function(n) {
  ret <- NA
  
  idx <- which(.spc.cached.constant.d2$n == n)
  if (length(idx) != 0) {
    ret <- .spc.cached.constant.d2$d2[idx]
  } 
  
  ret
}

