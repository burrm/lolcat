t.test.onesample<-function(x
                           ,null.hypothesis.mean = 0
                           ,alternative = c("two.sided","less","greater")
                           ,conf.level = 0.95
                           ,finite.population.N = NA
) {
  t.test.onesample.simple(sample.mean = mean(x)
                          ,sample.variance = var(x)
                          ,sample.size = length(na.omit(x))
                          ,null.hypothesis.mean = null.hypothesis.mean
                          ,alternative = alternative
                          ,conf.level = conf.level
                          ,finite.population.N = finite.population.N)
}