kurtosis.test.simple <-
  function(kurtosis
           ,sample.size
           ,input = c("fisher") #todo - pearson
           ,conf.level = .95
           ,alternative = c("two.sided","less","greater")
)   {
    n = as.double(sample.size)
    
    b2 <- convert.kurtosis(kurtosis, sample.size)
    E_b2 <- 3*(n-1)/(n+1)
    Var_b2 <- 24*n*(n-2)*(n-3)/( (n+3)*(n+5)*(n+1)^2 )
    
    standardized_b2 <- (b2 - E_b2)/sqrt(Var_b2)
    
    third_stand_moment_b2 <- (6*(n^2 - 5*n +2) /( (n+7)*(n+9) )) *
      sqrt( 6*(n+3)*(n+5) / (n*(n-2)*(n-3))  )
    
    A <- 6 + (8/ third_stand_moment_b2) * 
      ( 2/ third_stand_moment_b2 + sqrt(1 + 4/ third_stand_moment_b2^2) ) 
    
    Z_b2 <- ((1 - 2/(9*A)) - ((1-2/A) / (1+ standardized_b2 * sqrt(2/(A-4))))^(1/3) ) /sqrt(2/(9*A))
    
    p.value <- if (alternative[1] == "two.sided") {
      tmp<-pnorm(Z_b2)
      min(tmp,1-tmp)*2
    } else if (alternative[1] == "greater") {
      pnorm(Z_b2,lower.tail = FALSE)
    } else if (alternative[1] == "less") {
      pnorm(Z_b2,lower.tail = TRUE)
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
                 estimate    = c(kurtosis=ku, z=Z_b2, se.est=se.est, b2=b2),
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
    