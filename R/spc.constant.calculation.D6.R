spc.constant.calculation.D6 <- function(sample.size, n.sigma = 3) {

  ret <- NA
  if (sample.size < 11 & n.sigma == 3) {
    ret <-  if (sample.size == 2) {
      3.865
    }else if (sample.size == 3) {
      2.745
    }else if (sample.size == 4) {
      2.375
    }else if (sample.size == 5) {
      2.179
    }else if (sample.size == 6) {
      2.055
    }else if (sample.size == 7) {
      1.967
    }else if (sample.size == 8) {
      1.901
    }else if (sample.size == 9) {
      1.850
    }else if (sample.size == 10) {
      1.809
    } 
    
  } else { 
    ret <- 1 + n.sigma * (3.07/3) * spc.constant.calculation.d3(sample.size) / spc.constant.calculation.d4(sample.size)
  }
  
  ret
}
