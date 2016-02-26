.spc.cached.constant.c4 <- data.frame(
  sample.size = 2:15,
  D3 = c(.7979, .8862, .9213, .9400,
         .9515, .9594, .9650, .9693,
         .9727, .9754, .9776, .9794,
         .9810, .9823)
)

spc.constant.calculation.c4 <- function(sample.size) {
  ret <- NA
  
  idx <- which(.spc.cached.constant.c4$sample.size == sample.size)
  if (length(idx) != 0) {
    ret <- .spc.cached.constant.c4$c4[idx]
  } 
  
  ret
}