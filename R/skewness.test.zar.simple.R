#Zar 1999 version from Sheskin
#Equivalent to D'Agostino Skewness Test

skewness.test.zar.simple <-
  function(skewness
           ,sample.size
           ,input = c("fisher") #todo - pearson
           ,conf.level = .95
           ,alternative = c("two.sided","less","greater")
  )
  {
    
    n = sample.size
    sk      <- skewness
    se.est  <- sqrt(6*n*(n-1)/((n-2)*(n+1)*(n+3)))
    
    
    root.b1 <- convert.skewness(sk, n)
    
    A              <- root.b1 * sqrt((n+1)*(n+3) / (6*(n-2)))
    B              <- 3*(n^2+27*n-70)*(n+1)*(n+3) / ( (n-2)*(n+5)*(n+7)*(n+9)  )
    C              <- sqrt(2*(B-1)) -1
    D              <- sqrt(C)
    E              <- 1/sqrt(log(D))
    F.             <- A/sqrt(2/(C-1))
    
    z             <- E*log( F. + sqrt(F.^2 +1 ) )
    
    test.statistic <- z
    
    p.value <- if (alternative[1] == "two.sided") {
      tmp<-pnorm(z)
      min(tmp,1-tmp)*2
    } else if (alternative[1] == "greater") {
      pnorm(z,lower.tail = FALSE)
    } else if (alternative[1] == "less") {
      pnorm(z,lower.tail = TRUE)
    } else {
      NA
    }
    
    cv<-qnorm(conf.level+(1-conf.level)/2)
    
    estimate <- c(skewness=sk,z=test.statistic,se.est=se.est, root.b1 = root.b1)
    
    
    ciupper <- sk+cv*se.est
    cilower <- sk-cv*se.est
    
    retval<-list(data.name   = "input data",
                 statistic   = sk, 
                 estimate    = estimate,
                 parameter   = 0 ,
                 p.value     = p.value,
                 null.value  = 0,
                 alternative = "two.sided",
                 #method      = paste("D'Agostino Skewness Normality Test (", method[1], ")", sep=""),
                 method      = "D'Agostino Skewness Normality Test ",
                 conf.int    = c(cilower,ciupper)
    )
    
    #names(retval$estimate) <- c("sample skewness","z statistic")
    names(retval$statistic) <- "skewness"
    names(retval$null.value) <- "skewness"
    names(retval$parameter) <- "null hypothesis skewness"
    attr(retval$conf.int, "conf.level")  <- conf.level
    
    class(retval)<-"htest"
    retval
  }
