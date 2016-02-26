z.test.onesample<-function(x
                           ,known.population.variance
                           ,h0.mean
                           ,alternative = c("two.sided","less","greater")
                           ,conf.level = 0.95
                           ,finite.population.N = NA
) {
  z.test.onesample.simple(sample.mean = mean(x)
                          ,known.population.variance = known.population.variance
                          ,sample.size = length(na.omit(x))
                          ,h0.mean = h0.mean
                          ,alternative = alternative
                          ,conf.level = conf.level
                          ,finite.population.N = finite.population.N
                          )
  
}