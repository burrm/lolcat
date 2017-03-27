mean.z.test.onesample<-function(x
                           ,known.population.variance = var(x)
                           ,null.hypothesis.mean = 0
                           ,alternative = c("two.sided","less","greater")
                           ,conf.level = 0.95
                           ,finite.population.N = NA
                           ,na.rm = T
) {
  validate.htest.alternative(alternative = alternative)
  
  if (na.rm) {
    x <- na.omit(x)
  }
  
  mean.z.test.onesample.simple(sample.mean = mean(x)
                          ,known.population.variance = known.population.variance
                          ,sample.size = length(na.omit(x))
                          ,null.hypothesis.mean = null.hypothesis.mean
                          ,alternative = alternative
                          ,conf.level = conf.level
                          ,finite.population.N = finite.population.N
  )
  
}