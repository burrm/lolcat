variance.test.ksample.bartlett.box.fx <- function(
  fx           # a formula
  ,data = NULL # a data frame
  ,alternative = c("greater", "less", "two.sided")
) {
  validate.htest.alternative(alternative = alternative)
  
  input <- summary.impl(fx = fx, data=data, stat.n = T, stat.var = T)
  
  variance.test.ksample.bartlett.box.simple(
    sample.sizes = input$n
    ,sample.variances = input$var
    ,alternative = alternative
  )
  
}

