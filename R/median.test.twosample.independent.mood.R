median.test.twosample.independent.mood <- function(
  g1
  ,g2
  ,alternative = c("two.sided","less","greater")  
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  g1 <- na.omit(g1)
  g2 <- na.omit(g2)
  
  med.common <- median(c(g1,g2))
  
  n.above.g1 = length(which(g1 > med.common))
  n.equal.g1 = length(which(g1 == med.common))
  n.below.g1 = length(which(g1 < med.common))
  
  n.above.g2 = length(which(g2 > med.common))
  n.equal.g2 = length(which(g2 == med.common))
  n.below.g2 = length(which(g2 < med.common))
  
  median.test.twosample.independent.mood.simple(n.below.g1 = n.below.g1
                                                ,n.equal.g1 = n.equal.g1
                                                ,n.above.g1 = n.above.g1
                                                ,n.below.g2 = n.below.g2
                                                ,n.equal.g2 = n.equal.g2
                                                ,n.above.g2 = n.above.g2
                                                ,alternative = alternative
                                                ,conf.level = conf.level
  )
  
    
}