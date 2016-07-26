median.test.twosample.independent.mann.whitney <- function(
  g1
  ,g2
  ,alternative = c("two.sided","less","greater")  
) {
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

# http://www.real-statistics.com/non-parametric-tests/mann-whitney-test/
#  g1 <- c(11,15,9,4,34,17,18,14,12,13,26,31)
#  g2 <- c(34,31,35,29,28,12,18,30,14,22,10)
# median.test.twosample.independent.mann.whitney(g1,g2)
