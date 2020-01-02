#Equivalent to D'Agostino
#Presented in Sheskin

kurtosis.test.zar.simple <-
  function(kurtosis
           ,sample.size
           ,input = c("fisher") #todo - pearson
           ,conf.level = .95
           ,alternative = c("two.sided","less","greater")
  )   {
    validate.htest.alternative(alternative = alternative)
    n = as.double(sample.size)
    
    b2 <- convert.kurtosis(kurtosis, sample.size = sample.size)
    
    G <- 24*n*(n-2)*(n-3)/( (n+3)*(n+5)*(n+1)^2 )
    H <- (n-2)*(n-3)*(kurtosis)/((n+1)*(n-1)*sqrt(G))
    J <- (6*(n^2 - 5*n +2) /( (n+7)*(n+9) )) *
      sqrt( 6*(n+3)*(n+5) / (n*(n-2)*(n-3))  )
    K <- 6 + (8/J)*(2/J + sqrt(1+4/J^2)   )
    L <- (1- 2/K) / (1+H*sqrt(2/(K-4)))
    
    z <- (1-2/(9*K) - L^(1/3))/sqrt(2/(9*K))
    
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
    
    ku<-kurtosis
    
    cv<-qnorm(conf.level+(1-conf.level)/2)
    se.sk   <- sqrt(6*n*(n-1)/((n-2)*(n+1)*(n+3)))
    se.est  <- 2*se.sk*sqrt((n^2-1)/((n-3)*(n+5)))
    ciupper <- ku+cv*se.est
    cilower <- ku-cv*se.est
    
    retval<-list(data.name   = "input data",
                 statistic   = ku, 
                 estimate    = c(kurtosis=ku, z=z, se.est=se.est, b2=b2),
                 parameter   = 0 ,
                 p.value     = p.value,
                 null.value  = 0,
                 alternative = "two.sided",
                 method      = "D'Agostino Kurtosis Normality Test",
                 conf.int    = c(cilower,ciupper)
    )
    
    #names(retval$estimate) <- c("sample kurtosis","z statistic")
    names(retval$statistic) <- "kurtosis"
    names(retval$null.value) <- "kurtosis"
    names(retval$parameter) <- "null hypothesis kurtosis"
    attr(retval$conf.int, "conf.level")  <- conf.level
    
    class(retval)<-"htest"
    retval
    
  }
