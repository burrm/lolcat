#For known variances and sample sizes
variance.test.twosample.independent.simple<-function(sample.variance.g1
                                                     ,sample.size.g1
                                                     ,sample.variance.g2
                                                     ,sample.size.g2
                                                     ,alternative = c("two.sided","less","greater")
                                                     ,conf.level = 0.95) {
  validate.htest.alternative(alternative = alternative)
  
  df.g1 <- sample.size.g1 - 1
  df.g2 <- sample.size.g2 - 1
  
  f.statistic = sample.variance.g1/sample.variance.g2
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pf(f.statistic,df.g1,df.g2)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pf(f.statistic,df.g1,df.g2,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pf(f.statistic,df.g1,df.g2,lower.tail = TRUE)
  } else {
    NA
  }
  
  retval<-list(data.name   = "input variances and sample sizes",
               statistic   = f.statistic, 
               estimate    = c(sample.variance.g1 = sample.variance.g1
                               ,df.g1 = df.g1
                               ,sample.size.g1 = sample.size.g1
                               ,sample.variance.g2 = sample.variance.g2
                               ,df.g2 = df.g2
                               ,sample.size.g2 = sample.size.g2
                               ,power = power.variance.twosample.independent(
                                 variance.estimate.g1 = sample.variance.g1
                                 ,variance.estimate.g2 = sample.variance.g2
                                 ,sample.size.g1 = sample.size.g1
                                 ,sample.size.g2 = sample.size.g2
                                 ,alpha = 1-conf.level
                                 ,alternative = alternative
                                 ,details = F
                               )),
                              
               parameter   = 1,
               p.value     = p.value,
               null.value  = 1,
               alternative = alternative[1],
               method      = "Two-Sample F Test For Variance"
  )
  
  names(retval$statistic)  <- "F statistic"
  names(retval$null.value) <- "variance ratio"
  names(retval$parameter)  <- "variance ratio"
  #attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}
