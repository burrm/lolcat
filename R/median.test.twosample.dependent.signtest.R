median.test.twosample.dependent.signtest <- function(
  g1
  ,g2
  ,alternative = c("two.sided","less","greater")  
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  data <- na.omit(data.frame(g1 = g1, g2 = g2))
  g1 <- data$g1
  g2 <- data$g2
  
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
