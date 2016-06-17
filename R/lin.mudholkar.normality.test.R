#note - MVP returns either r abs(r) and two-tail p-value

lin.mudholkar.normality.test <- function(x
                                         ,method = c("lin-mudholkar-1980")
                                         ,alternative = c("two.sided","less","greater")
                                         ,conf.level = 0.95
                                         ) {
  x <- na.omit(x)
  
  n <- length(x)
  mean.i <- deleted.case.statistic(x)
  var.i  <- deleted.case.statistic(x, FUN = var)^(1/3)
  
  r <- cor(mean.i, var.i)
  
  lin.mudholkar.normality.test.simple(sample.r = r
                                      ,sample.size = n
                                      ,method = method
                                      ,alternative = alternative
                                      ,conf.level = conf.level
                                      )
}