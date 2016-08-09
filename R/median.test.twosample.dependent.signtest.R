median.test.twosample.dependent.signtest <- function(
  g1
  ,g2
  ,alternative = c("two.sided","less","greater")  
  ,conf.level = .95
) {
  
  g1 <- na.omit(g1)
  g2 <- na.omit(g2)
  
  d <- g1 - g2
  
  n.above <- length(which(d > 0))
  n.equal <- length(which(d == 0))   
  n.below <- length(which(d < 0))
  
  median.test.twosample.dependent.signtest.simple(
    n.above = n.above
    ,n.equal = n.equal
    ,n.below = n.below
    ,alternative = alternative
    ,conf.level = conf.level
  )  
  
}
