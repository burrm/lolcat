# d3 borrowed from SixSigma package
spc.constant.calculation.d3 <- function(sample.size) {
  d3.f <- function(x) {
    x * (1 - ptukey(x, sample.size, Inf))
  }
  
  d3.tmp <- integrate(d3.f, 0, Inf)$value*2
  
  #d3.tmp
  
  sqrt(d3.tmp-spc.constant.calculation.d2(sample.size)^2)
}



#spc.constant.calculation.d3(2)


spc.constant.calculation.D3 <- function(sample.size, n.sigma = 3, negative.as.NA = T) {
  ret <- 1 - n.sigma * spc.constant.calculation.d3(sample.size) / spc.constant.calculation.d2(sample.size)

  if (ret < 0 & negative.as.NA) {
    ret <- NA
  }
    
  ret
}
