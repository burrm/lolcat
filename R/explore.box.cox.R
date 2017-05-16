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
  x <- na.omit(x)
  orig.warnings <- unlist(options("warn"))
  options(warn = -1)
  
  all.lambda <- seq(lambda.min
                    ,lambda.max
                    ,step)

  ret <- data.frame(lambda = all.lambda)
    
  shift.x <- 0
  
  if (min(x) < 1 && correct.min) {
    shift.x <- 2*abs(min(x))
    #x <- x+shift.x
    
  } 
  
  ret$correct.min <- rep(shift.x, nrow(ret))
  
  ret$adtest.AA <- rep(NA, nrow(ret))
  ret$adtest.p <- rep(NA, nrow(ret))
  
  ret$swtest.W <- rep(NA, nrow(ret))
  ret$swtest.p <- rep(NA, nrow(ret))
  
  ret$g3.skewness <- rep(NA, nrow(ret))
  ret$g3test.z <- rep(NA, nrow(ret))
  ret$g3test.p <- rep(NA, nrow(ret))
  
  ret$g4.kurtosis <- rep(NA, nrow(ret))
  ret$g4test.z <- rep(NA, nrow(ret))
  ret$g4test.p <- rep(NA, nrow(ret))
  
  ret$pois.test.chi.square <- rep(NA, nrow(ret))
  ret$pois.test.p <- rep(NA, nrow(ret))
  
  ret$sw.exp.test.W <- rep(NA, nrow(ret))
  ret$sw.exp.test.p <- rep(NA, nrow(ret))
  
  n <- length(x)
  
  for (i in 1:nrow(ret)) {
    this.lambda <- ret$lambda[i]
    this.correct.min <- ret$correct.min[i]
    
    new.x <- transform.box.cox(x, lambda = this.lambda, correct.min = this.correct.min)
    
    new.x <- new.x[is.finite(x)]
    if (length(new.x) == n) {
    
      if (stat.ad.test) {
        t1 <- anderson.darling.normality.test(new.x)
        ret$adtest.AA[i] <- rmnames(t1$statistic)
        ret$adtest.p[i] <- t1$p.value
        rm(t1)
      }
      
      if (stat.sw.test) {
        t1 <- shapiro.test(new.x)
        ret$swtest.W[i] <- rmnames(t1$statistic)
        ret$swtest.p[i] <- t1$p.value
        rm(t1)
      }

      if (stat.skew.test) {
        t1 <- skewness.test(new.x)
        ret$g3.skewness[i] <- rmnames(t1$statistic)
        ret$g3test.z[i] <- rmnames(t1$estimate[2])
        ret$g3test.p[i] <- t1$p.value
        rm(t1)
      }
      
      if (stat.kurt.test) {
        t1 <- kurtosis.test(new.x)
        ret$g4.kurtosis[i] <- rmnames(t1$statistic)
        ret$g4test.z[i] <- rmnames(t1$estimate[2])
        ret$g4test.p[i] <- t1$p.value
        rm(t1)
      }
      
      if (stat.pois.dist.test) {
        t1 <- poisson.dist.test(new.x)
        ret$pois.test.chi.square[i] <- rmnames(t1$statistic)
        ret$pois.test.p[i] <- t1$p.value
        rm(t1)
      }
      
      if (stat.sw.exp.test) {
        t1 <- shapiro.wilk.exponentiality.test(new.x)
        ret$sw.exp.test.W[i] <- rmnames(t1$statistic) 
        ret$sw.exp.test.p[i] <- t1$p.value
        rm(t1)
      }
    
    }
  }

  
  if (!stat.ad.test) {
    ret$adtest.AA <- NULL
    ret$adtest.p  <- NULL
  }
  
  if (!stat.sw.test) {
    ret$swtest.W <- NULL
    ret$swtest.p <- NULL
  }
  
  if (!stat.skew.test) {
    ret$g3.skewness <- NULL
    ret$g3test.z    <- NULL
    ret$g3test.p    <- NULL
  }
  
  if (!stat.kurt.test) {
    ret$g4.kurtosis <- NULL
    ret$g4test.z    <- NULL
    ret$g4test.p    <- NULL
  }
  
  if (!stat.pois.dist.test) {
    ret$pois.test.chi.square <- NULL
    ret$pois.test.p          <- NULL
  }
  
  if (!stat.sw.exp.test) {
    ret$sw.exp.test.W <- NULL 
    ret$sw.exp.test.p <- NULL
  }
  
  options(warn = orig.warnings)
  ret
}