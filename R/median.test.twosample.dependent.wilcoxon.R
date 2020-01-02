median.test.twosample.dependent.wilcoxon <- function(
  g1
  ,g2
  ,alternative = c("two.sided","less","greater")  
) {
  validate.htest.alternative(alternative = alternative)
  
  g1 <- na.omit(g1)
  g2 <- na.omit(g2)
  
  d <- g1 - g2
  
  d <- d[which(abs(d) != 0)]
  
  rank_d <- rank(abs(d))
  
  adj.sample.size    <- 0
  sum.ranks.positive <- 0
  sum.ranks.negative <- 0
  
  idx <- which(d > 0)
  adj.sample.size <- adj.sample.size + length(idx)
  if (length(idx) > 0) {
    sum.ranks.positive <- sum(rank_d[idx])
  }
  
  idx <- which(d < 0)
  adj.sample.size <- adj.sample.size + length(idx)
  if (length(idx) > 0) {
    sum.ranks.negative <- sum(rank_d[idx])
  }

  median.test.twosample.dependent.wilcoxon.simple(sum.ranks.positive =sum.ranks.positive
                                        ,sum.ranks.negative =  sum.ranks.negative
                                        ,sample.size = adj.sample.size
                                        ,alternative = alternative
  )
  

}

wilcoxon.signed.ranks.twosample.test <- median.test.twosample.dependent.wilcoxon

#median.test.twosample.dependent(c(9,2,1,4,6,4,7,8,5,1), c(8,2,3,2,3,0,4,5,4,0))
