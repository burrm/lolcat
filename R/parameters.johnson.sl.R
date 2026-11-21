parameters.johnson.sl <- function(z
                                  ,quantile.neg.3z
                                  ,quantile.neg.z
                                  ,quantile.z
                                  ,quantile.3z) {
  
  ret <- list()
  
  est.m <- quantile.3z - quantile.z
  est.n <- quantile.neg.z - quantile.neg.3z
  est.p <- quantile.z - quantile.neg.z
  
  #Johnson SL parameter estimates (Slifker/Shapiro 1980)
  
  ret$eta <- (2*z)/log(est.m/est.p)
  
  ret$gamma <- ret$eta * log((est.m/est.p -1) / (est.p * sqrt(est.m/est.p)))  
  
  ret$lambda <- 0
  
  ret$epsilon <- .5*(quantile.z + quantile.neg.z) - (est.p/2)*((est.m/est.p+1)/(est.m/est.p-1))
  

  ret$p.over.n <- est.p/est.n
  ret$p.over.m <- est.p/est.m
  
  ret$m <- est.m
  ret$n <- est.n
  ret$p <- est.p
  
  ret
}