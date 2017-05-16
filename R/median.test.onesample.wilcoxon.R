median.test.onesample.wilcoxon <- function(
  x
  ,null.hypothesis.location = 0
  ,alternative = c("two.sided","less","greater")
) {
  validate.htest.alternative(alternative = alternative)
  
  x <- na.omit(x)
  d <- x - null.hypothesis.location
  
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
  
  median.test.onesample.wilcoxon.simple(sum.ranks.positive =sum.ranks.positive
                                        ,sum.ranks.negative =  sum.ranks.negative
                                        ,adj.sample.size = adj.sample.size
                                        ,null.hypothesis.location = null.hypothesis.location
                                        ,alternative = alternative
                                        )
    
}


wilcoxon.signed.ranks.onesample.test <- median.test.onesample.wilcoxon