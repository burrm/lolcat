.spc.cached.constant.D3 <- data.frame(
  n = 2:15,
  D3 = c(NA,NA,NA,NA,NA,
         .076, .136, .184, .223,
         .256, .283, .307, .328,
         .347)
)

spc.constant.calculation.D3 <- function(n) {
  ret <- NA
  
  idx <- which(.spc.cached.constant.D3$n == n)
  if (length(idx) != 0) {
    ret <- .spc.cached.constant.D3$D3[idx]
  } 
  
  ret
}





.spc.cached.constant.d3 <- data.frame(
  n = 2:15,
  d3 = c(.853, .888, .880, .864,
         .848, .833, .820, .808,
         .797, .787, .778, .770,
         .763, .756)
)

spc.constant.calculation.d3 <- function(n) {
  ret <- NA
  
  idx <- which(.spc.cached.constant.d3$n == n)
  if (length(idx) != 0) {
    ret <- .spc.cached.constant.d3$d3[idx]
  } 
  
  ret
}