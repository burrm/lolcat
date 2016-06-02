skewness.test <-
function(x
         ,conf.level = .95
         #,method = c("t","z")
         )
{
    x <- na.omit(x)
  
    n = length(x)
    sk<-skewness(x)
    se.est  <- sqrt(6*n*(n-1)/((n-2)*(n+1)*(n+3)))
    
    #if (method[1] == "z") {
      #Modified skewness test from fBasics
      meanX = mean(x)
      s =  sqrt(mean((x-meanX)**2))
      a3 = mean((x-meanX)**3)/s**3
      SD3 = sqrt(6*(n-2)/((n+1)*(n+3)))
      U3 = a3/SD3
      b  = (3*(n**2+27*n-70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
      W2 = sqrt(2*(b-1))-1
      delta = 1/sqrt(log(sqrt(W2)))
      a = sqrt(2/(W2-1))
      Z3 = delta*log((U3/a)+sqrt((U3/a)**2+1))
      pZ3 = 2*(1-pnorm(abs(Z3),0,1))
      
      test.statistic <- Z3
      p.value <- pZ3
      cv<-qnorm(conf.level+(1-conf.level)/2)
      
      estimate <- c(skewness=sk,z=test.statistic,se.est=se.est)
      
    # } else if (method[1] == "t") {
    #   #Skewness t (D'Agostino and Tietjen, 1971)
    #   m3   <- .moment.m3(x)
    #   m2   <- .moment.m2(x)
    #   beta <- 3*(n^2+27*n-70)*(n+1)*(n+3)/((n-2)*(n+5)*(n+7)*(n+9))
    #   df   <- (4*beta-6)/(beta-3)
    #   
    #   test.statistic <- (m3/m2^(3/2))*((n+1)*(n+3)/(6*(n-2)))*sqrt(df/(df-2))
    #   p.value <- pt(test.statistic, df = df)
    #   p.value<-2*min(p.value,1-p.value)
    #   
    #   cv<-qt(conf.level+(1-conf.level)/2, df = df)
    #   
    #   estimate <- c(skewness=sk,t=test.statistic, df = df, se.est=se.est)
    #   
    #   
    # } else {
    #   stop("Unknown method for skewness.test")
    # }
    
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
