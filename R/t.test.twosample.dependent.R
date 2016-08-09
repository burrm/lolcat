t.test.twosample.dependent<-function(
  x1
  ,x2
  ,null.hypothesis.difference = 0
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95) {
  
  d <- x1 - x2
  
  d <- na.omit(d)
  pair.differences.mean <- mean(d)
  pair.differences.variance <- var(d)
  sample.size <- length(d)
  
  t.test.twosample.dependent.simple.dbar(
    pair.differences.mean = pair.differences.mean
    ,pair.differences.variance = pair.differences.variance
    ,sample.size = sample.size
    ,null.hypothesis.difference = null.hypothesis.difference
    ,alternative = alternative
    ,conf.level = conf.level
  )
}