median.test.twosample.dependent.wilcoxon.simple <- function(
  sum.ranks.positive = 0
  ,sum.ranks.negative = 0
  ,sample.size = NA
  ,alternative = c("two.sided","less","greater")  
) {
  
  t <- median.test.onesample.wilcoxon.simple(
    sum.ranks.positive = sum.ranks.positive
    ,sum.ranks.negative = sum.ranks.negative
    ,adj.sample.size = sample.size
    ,null.hypothesis.location = 0
    ,alternative = alternative  
    
  )
  
  names(t$null.value) <- "difference"
  names(t$parameter) <- "null hypothesis difference"
  
  t
  
}

wilcoxon.signed.ranks.twosample.test.simple <- median.test.twosample.dependent.wilcoxon.simple

