variability.test.twosample.independent.siegel.tukey.simple <- function(
  sum.ranks.g1
  ,sum.ranks.g2
  ,sample.size.g1
  ,sample.size.g2
  ,alternative = c("two.sided","less","greater")  
) {
  validate.htest.alternative(alternative = alternative)
  
  t.out <- median.test.twosample.independent.mann.whitney.simple(
    sum.ranks.g1 = sum.ranks.g1
    ,sum.ranks.g2 = sum.ranks.g2
    ,sample.size.g1 = sample.size.g1
    ,sample.size.g2 = sample.size.g2
    ,alternative = alternative
  )
  
  t.out$method <- "Siegel-Tukey Test for Equal Variability"
  names(t.out$null.value) <- "variability difference"
  names(t.out$parameter) <- "null hypothesis variability difference"
  t.out
}