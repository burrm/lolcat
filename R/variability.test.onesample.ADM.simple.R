variability.test.onesample.ADM.simple <-function(
  sample.mean.ADM
  ,sample.variance.ADM
  ,sample.size
  ,null.hypothesis.mean.ADM = 0
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95
  ,finite.population.N = NA
) {
  validate.htest.alternative(alternative)
  
  retval <- mean.t.test.onesample.simple(sample.mean = sample.mean.ADM
                                         ,sample.variance = sample.variance.ADM
                                         ,sample.size = sample.size
                                         ,null.hypothesis.mean = null.hypothesis.mean.ADM
                                         ,alternative = alternative
                                         ,conf.level = conf.level
                                         ,finite.population.N = finite.population.N)
  
  retval$method <- "One-Sample t Test For Average Median Absolute Deviation"
  names(retval$null.value) <- "mean ADM"
  names(retval$parameter) <- "null hypothesis mean ADM"
  
  
  retval
  
}

variability.test.onesample.brown.forsythe.simple <- variability.test.onesample.ADM.simple