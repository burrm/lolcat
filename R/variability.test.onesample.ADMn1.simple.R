variability.test.onesample.ADMn1.simple <-function(
  sample.mean.ADMn1
  ,sample.variance.ADMn1
  ,sample.size
  ,null.hypothesis.mean.ADMn1 = 0
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95
  ,finite.population.N = NA
) {
  validate.htest.alternative(alternative)
  
  retval <- mean.t.test.onesample.simple(sample.mean = sample.mean.ADMn1
                                         ,sample.variance = sample.variance.ADMn1
                                         ,sample.size = sample.size
                                         ,null.hypothesis.mean = null.hypothesis.mean.ADMn1
                                         ,alternative = alternative
                                         ,conf.level = conf.level
                                         ,finite.population.N = finite.population.N)
  
  retval$method <- "One-Sample t Test For Average Median Absolute Deviation (Midpoint Removed)"
  names(retval$null.value) <- "mean ADMn-1"
  names(retval$parameter) <- "null hypothesis mean ADMn-1"
  
  
  retval
  
}

variability.test.onesample.petrovich.simple <- variability.test.onesample.ADMn1.simple