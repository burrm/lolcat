.spc.cached.constant.c4 <- data.frame(
  n = 2:15,
  D3 = c(.7979, .8862, .9213, .9400,
         .9515, .9594, .9650, .9693,
         .9727, .9754, .9776, .9794,
         .9810, .9823)
)

spc.constant.calculation.c4 <- function(n) {
  ret <- NA
  
  idx <- which(.spc.cached.constant.c4$n == n)
  if (length(idx) != 0) {
    ret <- .spc.cached.constant.c4$c4[idx]
  } 
  
  ret
}