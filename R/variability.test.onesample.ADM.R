variability.test.onesample.ADM <-function(
  x
  ,null.hypothesis.mean.ADM = 0
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95
  ,finite.population.N = NA
  ,na.rm = T
) {
  validate.htest.alternative(alternative)
  
  if (na.rm) {
    x <- na.omit(x)
  }
  
  adm <- dispersion.ADM(x)
  
  variability.test.onesample.ADM.simple(
    sample.mean.ADM = mean(adm)
    ,sample.variance.ADM = var(adm)
    ,sample.size = length(adm)
    ,null.hypothesis.mean.ADM = null.hypothesis.mean.ADM
    ,alternative = alternative
    ,conf.level = conf.level
    ,finite.population.N = finite.population.N
  )
  
}