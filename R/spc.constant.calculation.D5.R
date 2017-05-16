spc.constant.calculation.D5 <- function(sample.size, n.sigma = 3, negative.as.NA = T) {
  ret <- NA
  if (sample.size < 11 & n.sigma == 3) {
    ret <-  if (sample.size == 2) {
      NA
    }else if (sample.size == 3) {
      NA
    }else if (sample.size == 4) {
      NA
    }else if (sample.size == 5) {
      NA
    }else if (sample.size == 6) {
      NA
    }else if (sample.size == 7) {
      0.078
    }else if (sample.size == 8) {
      0.139
    }else if (sample.size == 9) {
      0.187
    }else if (sample.size == 10) {
      0.227
    } 
    
  } else {
  
  ret <- 1 - n.sigma * (3.07/3) * spc.constant.calculation.d3(sample.size) / spc.constant.calculation.d4(sample.size)
  
  }
  if (!is.na(ret)) {
    if (ret < 0 & negative.as.NA) {
      ret <- NA
    }
  }
  
  ret
}
