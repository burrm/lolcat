parameters.johnson.sb <- function(z
                                  ,quantile.neg.3z
                                  ,quantile.neg.z
                                  ,quantile.z
                                  ,quantile.3z) {
  
  ret <- list()
  
  est.m <- quantile.3z - quantile.z
  est.n <- quantile.neg.z - quantile.neg.3z
  est.p <- quantile.z - quantile.neg.z
  
  #Johnson SB parameter estimates (Slifker/Shapiro 1980)
  
  ret$eta <- z / ( acosh(.5*sqrt((1+est.p/est.m)*(1+est.p/est.n))) )
  
  
  ret$gamma <- ret$eta * asinh(
    ((est.p/est.n - est.p/est.m) * sqrt((1+est.p/est.m)*(1+est.p/est.n) -4)  ) /
      (2*((est.p/est.m)*(est.p/est.n) -1))
  )
  
  
  ret$lambda <-  (est.p * sqrt( ((1+est.p/est.m)*(1+est.p/est.n) -2)^2  -4) /
                             ((est.p/est.m)*(est.p/est.n) -1))
  
  
  ret$epsilon <- (.5*(quantile.z + quantile.neg.z) - ret$lambda/2 + 
                             (est.p*(est.p/est.n - est.p/est.m)) / (2*((est.p/est.m)*(est.p/est.n) -1))
  )
  
  
  ret$m <- est.m
  ret$n <- est.n
  ret$p <- est.p
  
  # ret$p.over.n <- est.p/est.n
  # ret$p.over.m <- est.p/est.m
  
  ret
}