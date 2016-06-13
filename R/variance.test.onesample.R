variance.test.onesample<-function(g1
                                  ,h0.variance = 1
                                  ,alternative = c("two.sided","less","greater")
                                  ,conf.level = 0.95) {
  
  g1 <- na.omit(g1)
  sample.variance <- var(g1)
  sample.size <- length(g1)
  
  variance.test.onesample.simple(sample.variance
                                 ,sample.size
                                 ,h0.variance = h0.variance
                                 ,alternative = alternative
                                 ,conf.level = conf.level)
  
}