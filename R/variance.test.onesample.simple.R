#Simple interface for a given variance and sample size
variance.test.onesample.simple<-function(sample.variance
                                         ,sample.size
                                         ,h0.variance = 1
                                         ,alternative = c("two.sided","less","greater")
                                         ,conf.level = 0.95) {
  
  df = sample.size - 1
  chilower = qchisq((1 - conf.level)/2, df)
  chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
  v = sample.variance
  chi.square.statistic = df*v/h0.variance
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pchisq(chi.square.statistic,df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pchisq(chi.square.statistic,df,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pchisq(chi.square.statistic,df,lower.tail = TRUE)
  } else {
    NA
  }
  
  retval<-list(data.name   = "sample variance and sample size",
               statistic   = chi.square.statistic, 
               estimate    = c(v,df,sample.size),
               parameter   = h0.variance,
               p.value     = p.value,
               null.value  = h0.variance,
               alternative = alternative[1],
               method      = "One-Sample Chi-Square Test For Variance",
               conf.int    = c(df*v/chiupper,df*v/chilower)
  )
  
  names(retval$estimate) <- c("sample.variance","df","sample.size")
  names(retval$statistic) <- "chi-square statistic"
  names(retval$null.value) <- "variance"
  names(retval$parameter) <- "null hypothesis variance"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}