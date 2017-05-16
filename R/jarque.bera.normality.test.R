jarque.bera.normality.test <-
  function(x
           ,conf.level = .95
           ,alternative = c("greater","two.sided","less")
  )
  {
    validate.htest.alternative(alternative = alternative)
    x <- na.omit(x)
    
    fisher.g1   <- skewness(x)
    fisher.g2   <- kurtosis(x)
    sample.size <- length(x)
    
    jarque.bera.normality.test.simple(skewness    =  fisher.g1,
                                      kurtosis    = fisher.g2,
                                      sample.size = sample.size,
                                      conf.level  = conf.level,
                                      alternative = alternative)
    
  }
