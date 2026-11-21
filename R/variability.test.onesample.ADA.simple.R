variability.test.onesample.ADA.simple <-function(
   sample.mean.ADA
  ,sample.variance.ADA
  ,sample.size
  ,null.hypothesis.mean.ADA = 0
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95
  ,finite.population.N = NA
) {
  validate.htest.alternative(alternative)
  
  retval <- mean.t.test.onesample.simple(sample.mean = sample.mean.ADA
                               ,sample.variance = sample.variance.ADA
                               ,sample.size = sample.size
                               ,null.hypothesis.mean = null.hypothesis.mean.ADA
                               ,alternative = alternative
                               ,conf.level = conf.level
                               ,finite.population.N = finite.population.N)
  
  retval$method <- "One-Sample t Test For Average Mean Absolute Deviation"
  names(retval$null.value) <- "mean ADA"
  names(retval$parameter) <- "null hypothesis mean ADA"
  
  
  retval
  
}

variability.test.onesample.levene.simple <- variability.test.onesample.ADA.simple