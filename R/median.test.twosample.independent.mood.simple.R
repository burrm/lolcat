median.test.twosample.independent.mood.simple <- function(
  n.below.g1 = 0
  ,n.equal.g1 = 0 #Future use...
  ,n.above.g1 = 0
  
  ,n.below.g2 = 0
  ,n.equal.g2 = 0 #Future use...
  ,n.above.g2 = 0
  
  #,include.ties = F - TODO: Better Tie Behavior...
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95
) {
  validate.htest.alternative(alternative = alternative)
  n1 <- n.below.g1 + n.above.g1
  n2 <- n.below.g2 + n.above.g2
  
  t <- proportion.test.twosample.exact.simple(sample.proportion.g1 = n.above.g1 / n1
                                              ,sample.size.g1 = n1
                                              ,sample.proportion.g2 = n.above.g2 / n2
                                              ,sample.size.g2 = n2
                                              ,alternative = alternative
                                              ,conf.level = conf.level
                                              )
  
  
  t
  
}


mood.test.simple <- median.test.twosample.independent.mood.simple

