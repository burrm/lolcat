median.test.twosample.independent.mann.whitney <- function(
  g1
  ,g2
  ,alternative = c("two.sided","less","greater")  
) {
  validate.htest.alternative(alternative = alternative)
  g1 <- na.omit(g1)
  g2 <- na.omit(g2)
  
  ranks <- rank(c(g1,g2))
  ranks.g1 <- ranks[1:length(g1)]
  ranks.g2 <- ranks[(length(g1)+1):length(ranks)]
  
  median.test.twosample.independent.mann.whitney.simple(
    sum.ranks.g1 = sum(ranks.g1)
    ,sum.ranks.g2 = sum(ranks.g2)
    ,sample.size.g1 = length(ranks.g1)
    ,sample.size.g2 = length(ranks.g2)
    ,alternative = alternative
  )
}

