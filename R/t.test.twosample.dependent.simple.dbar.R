t.test.twosample.dependent.simple.dbar<-function(
  pair.differences.mean
  ,pair.differences.variance
  ,n
  ,h0.difference = 0
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95) {
  
  ret<-t.test.onesample.simple(pair.differences.mean,
                               pair.differences.variance,
                               n,
                               h0.difference,
                               alternative,
                               conf.level)
  
  ret$method <- "Dependent Samples t Test for Means (D-bar method)"
  
  ret
}