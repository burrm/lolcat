explore.box.cox <- function(x
                            ,lambda.min = -5
                            ,lambda.max = 5
                            ,step = .1
                            
                            #Shift data for possible exponent issues
                            ,correct.min = T
                            
                            #Tests to return
                            ,stat.ad.test   = F
                            ,stat.sw.test   = F
                            ,stat.skew.test = T
                            ,stat.kurt.test = T
                            ,stat.pois.dist.test = F
                            ,stat.sw.exp.test = F
                            
                            ) {

  all.lambda <- seq(lambda.min
                    ,lambda.max
                    ,step)

  ret <- data.frame(lambda = all.lambda)
    
  shift.x <- 0
  
  if (min(x) < 1 && correct.min) {
    shift.x <- 2*abs(min(x))
    x <- x+shift.x
    
  } 
  
  ret$correct.min <- rep(shift.x, nrow(ret))
  
  ss <- as.data.frame(t(sapply(all.lambda, FUN = function(lambda) { 
    new.x <- transform.box.cox(x, lambda, shift.x)
      ret <- list()
  
      if (stat.ad.test) {
        t1 <- anderson.darling.test(new.x)
        ret$adtest.AA <- rmnames(t1$statistic)
        ret$adtest.p <- t1$p.value
      }
      
      if (stat.sw.test) {
        t1 <- shapiro.test(new.x)
        ret$swtest.W <- rmnames(t1$statistic)
        ret$swtest.p <- t1$p.value
      }
          
      if (stat.skew.test) {
        t1 <- skewness.test(new.x)
        ret$g3.skewness <- rmnames(t1$statistic)
        ret$g3test.z <- rmnames(t1$estimate[2])
        ret$g3test.p <- t1$p.value
      }
      
      if (stat.kurt.test) {
        t1 <- kurtosis.test(new.x)
        ret$g4.kurtosis <- rmnames(t1$statistic)
        ret$g4test.z <- rmnames(t1$estimate[2])
        ret$g4test.p <- t1$p.value
      }
      
      if (stat.pois.dist.test) {
        t1 <- poisson.dist.test(new.x)
        ret$pois.test.chi.square <- rmnames(t1$statistic)
        ret$pois.test.p <- t1$p.value
      }
      
      if (stat.sw.exp.test) {
        t1 <- shapiro.wilk.exponentiality.test(new.x)
        ret$sw.exp.test.W <- rmnames(t1$statistic) 
        ret$sw.exp.test.p <- t1$p.value
      }
      
      
      ret
    })))
    
  ret <-cbind(ret, ss)
  for (i in names(ret)) {
    if (is.list(ret[[i]])) {
      ret[[i]] <- unlist(ret[[i]])
    }
  }
  
  ret
}