# d3 borrowed from SixSigma package
spc.constant.calculation.d3 <- function(sample.size) {
  sapply(sample.size, FUN = function(i) {
    d3.f <- function(x) {
      x * (1 - ptukey(x, i, Inf))
    }
  
    d3.tmp <- integrate(d3.f, 0, Inf)$value*2
  
    #d3.tmp
 
    sqrt(d3.tmp-spc.constant.calculation.d2(i)^2)
  })


}



#spc.constant.calculation.d3(2)


spc.constant.calculation.D3 <- function(sample.size, n.sigma = 3, negative.as.NA = T) {
  sapply(sample.size, FUN = function(i) {
      ret <- 1 - n.sigma * spc.constant.calculation.d3(i) / spc.constant.calculation.d2(i)

      if (ret < 0 & negative.as.NA) {
        ret <- NA
      }
        
      ret
  })
  

}
