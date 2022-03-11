#' @rdname z.test.onesample.simple
z.test.onesample<-function(x
                           ,known.population.variance
                           ,null.hypothesis.mean
                           ,alternative = c("two.sided","less","greater")
                           ,conf.level = 0.95
                           ,finite.population.N = NA
) {
  validate.htest.alternative(alternative = alternative)
  z.test.onesample.simple(sample.mean = mean(x)
                          ,known.population.variance = known.population.variance
                          ,sample.size = length(na.omit(x))
                          ,null.hypothesis.mean = null.hypothesis.mean
                          ,alternative = alternative
                          ,conf.level = conf.level
                          ,finite.population.N = finite.population.N
                          )
  
}