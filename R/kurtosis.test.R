kurtosis.test <-
  function(x
           ,conf.level = .95
           ,alternative = c("two.sided","less","greater")
           #,type = 2
  )
  {
    validate.htest.alternative(alternative = alternative)
    x <- na.omit(x)
    
    fisher.g2 <- kurtosis(x)
    sample.size <- length(x)
    
    kurtosis.test.simple(kurtosis =  fisher.g2,
                         sample.size = sample.size,
                         conf.level = conf.level,
                         alternative = alternative)
    
  }