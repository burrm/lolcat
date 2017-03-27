variability.test.twosample.independent.siegel.tukey <- function(
  g1
  ,g2
  ,alternative = c("two.sided","less","greater")
  ,location.adjust = NULL #NULL or centering function, typically center.median
) {
  validate.htest.alternative(alternative = alternative)
  
  if (!is.null(location.adjust)) {
    g1 <- location.adjust(g1)
    g2 <- location.adjust(g2)
  }
  
  ranks <- rank.siegel.tukey(g1 = g1
                             ,g2 = g2
                             ,details = F)
  ranks.g1 <- ranks[[1]]
  ranks.g2 <- ranks[[2]]
  
  t.out <- variability.test.twosample.independent.siegel.tukey.simple(
    sum.ranks.g1 = sum(ranks.g1)
    ,sum.ranks.g2 = sum(ranks.g2)
    ,sample.size.g1 = length(ranks.g1)
    ,sample.size.g2 = length(ranks.g2)
    ,alternative = alternative
  )
  
  t.out
}
  
