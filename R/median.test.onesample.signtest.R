median.test.onesample.signtest <- function(
  x
  ,h0.location = 0
  ,alternative = c("two.sided","less","greater")
) {
  
  x <- na.omit(x)
  d <- x - h0.location

  n.above = length(which(d > 0))  
  n.equal = length(which(d == 0))  
  n.below = length(which(d < 0))  
  
  median.test.onesample.signtest.simple(n.below = n.below
                                        ,n.equal = n.equal
                                        ,n.above = n.above
                                        ,h0.location = h0.location
                                        ,alternative = alternative
                                        ,conf.level = conf.level
                                        )
  
}