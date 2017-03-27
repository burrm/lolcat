median.test.twosample.dependent.signtest.simple <- function(
  n.below = 0
  ,n.equal = 0 #Future use...
  ,n.above = 0
  #,include.ties = F - TODO: Better Tie Behavior...
  ,alternative = c("two.sided","greater","less")
  ,conf.level = 0.95
  #,location.name = c("median", "mean")
) {
  validate.htest.alternative(alternative = alternative)
  
  n <- n.below + n.above
  
  b.test <- proportion.test.onesample.exact.simple(sample.proportion = n.above/n
                                                   ,null.hypothesis.proportion = .5
                                                   ,sample.size = n
                                                   ,alternative = alternative
                                                   ,conf.level = conf.level
  )
  
  b.test
}

#median.test.onesample.signtest.simple(n.above = 3, n.below = 6)
