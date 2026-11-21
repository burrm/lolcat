#' @rdname mean.t.test.onesample.simple
mean.t.test.onesample<-function(
  x
  ,null.hypothesis.mean = 0
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95
  ,finite.population.N = NA
  ,na.rm = T
) {
  if (na.rm) {
    x <- na.omit(x)
  }
  
  mean.t.test.onesample.simple(sample.mean = mean(x)
                          ,sample.variance = var(x)
                          ,sample.size = length(x)
                          ,null.hypothesis.mean = null.hypothesis.mean
                          ,alternative = alternative
                          ,conf.level = conf.level
                          ,finite.population.N = finite.population.N)
}