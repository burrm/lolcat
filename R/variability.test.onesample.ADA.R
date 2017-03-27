variability.test.onesample.ADA <-function(
  x
  ,null.hypothesis.mean.ADA = 0
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95
  ,finite.population.N = NA
  ,na.rm = T
) {
  validate.htest.alternative(alternative)
  
  if (na.rm) {
    x <- na.omit(x)
  }
  
  ada <- dispersion.ADA(x)
  
  variability.test.onesample.ADA.simple(
    sample.mean.ADA = mean(ada)
    ,sample.variance.ADA = var(ada)
    ,sample.size = length(ada)
    ,null.hypothesis.mean.ADA = null.hypothesis.mean.ADA
    ,alternative = alternative
    ,conf.level = conf.level
    ,finite.population.N = finite.population.N
  )
    
}