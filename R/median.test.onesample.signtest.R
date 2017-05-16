median.test.onesample.signtest <- function(
  x
  ,null.hypothesis.location = 0
  ,alternative = c("greater","two.sided","less")
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  x <- na.omit(x)
  d <- x - null.hypothesis.location

  n.above = length(which(d > 0))  
  n.equal = length(which(d == 0))  
  n.below = length(which(d < 0))  
  
  median.test.onesample.signtest.simple(n.below = n.below
                                        ,n.equal = n.equal
                                        ,n.above = n.above
                                        ,null.hypothesis.location = null.hypothesis.location
                                        ,alternative = alternative
                                        ,conf.level = conf.level
                                        )
  
}