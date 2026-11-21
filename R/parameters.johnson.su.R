parameters.johnson.su <- function(z
                                  ,quantile.neg.3z
                                  ,quantile.neg.z
                                  ,quantile.z
                                  ,quantile.3z) {
  
  ret <- list()
  
  est.m <- quantile.3z - quantile.z
  est.n <- quantile.neg.z - quantile.neg.3z
  est.p <- quantile.z - quantile.neg.z
  
  #Johnson SU parameter estimates (Slifker/Shapiro 1980)
  
  ret$eta <- 2*z / acosh(.5*(est.m/est.p + est.n/est.p))
  
  ret$gamma <- ret$eta*asinh(
    (est.n / est.p - est.m / est.p) /
      (2*sqrt( (est.m/est.p)*(est.n/est.p) -1  ) )
  )
  
  ret$lambda <- (2*est.p*sqrt( (est.m/est.p)*(est.n/est.p) -1  ) / 
                            ( (est.m/est.p +est.n /est.p -2 ) *sqrt((est.m/est.p +est.n /est.p +2 )))
  )
  
  ret$epsilon <- ((quantile.z +quantile.neg.z)/2 +
                             est.p*(est.n/est.p -est.m/est.p) / ( 2*(est.m/est.p +est.n/est.p -2)  )
                           
  )
  
  ret$m <- est.m
  ret$n <- est.n
  ret$p <- est.p
  
  ret
}