t.test.onesample<-function(x
                           ,h0.mean = 0
                           ,alternative = c("two.sided","less","greater")
                           ,conf.level = 0.95
                           ,finite.population.N = NA
) {
  t.test.onesample.simple(sample.mean = mean(sample)
                          ,sample.variance = var(x)
                          ,sample.size = length(na.omit(x))
                          ,h0.mean = h0.mean
                          ,alternative = alternative
                          ,conf.level = conf.level
                          ,finite.population.N = finite.population.N)
}