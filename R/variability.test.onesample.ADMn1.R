variability.test.onesample.ADMn1 <-function(
  x
  ,null.hypothesis.mean.ADMn1 = 0
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95
  ,finite.population.N = NA
  ,na.rm = T
) {
  validate.htest.alternative(alternative)
  
  if (na.rm) {
    x <- na.omit(x)
  }
  
  adm <- dispersion.ADMn1(x)

  adm <- na.omit(adm)
    
  variability.test.onesample.ADMn1.simple(
    sample.mean.ADMn1 = mean(adm)
    ,sample.variance.ADMn1 = var(adm)
    ,sample.size = length(adm)
    ,null.hypothesis.mean.ADMn1 = null.hypothesis.mean.ADMn1
    ,alternative = alternative
    ,conf.level = conf.level
    ,finite.population.N = finite.population.N
  )
  
}