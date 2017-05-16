#Simple interface for a given variance and sample size
variance.test.onesample.simple<-function(sample.variance
                                         ,sample.size
                                         ,null.hypothesis.variance = 1
                                         ,alternative = c("two.sided","greater", "less")
                                         ,conf.level = 0.95) {
  validate.htest.alternative(alternative = alternative)
  
  df = sample.size - 1
  chilower = qchisq((1 - conf.level)/2, df)
  chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
  v = sample.variance
  chi.square.statistic = df*v/null.hypothesis.variance
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
               estimate    = c(sample.variance = v,
                               df = df
                               ,sample.size = sample.size
                               ,power = power.variance.onesample(
                                 sample.size =sample.size
                                 ,null.hypothesis.variance = null.hypothesis.variance
                                 ,alternative.hypothesis.variance = v
                                 ,alpha = 1-conf.level
                                 ,alternative = alternative
                                 ,details=F
                               )
                               ),
               parameter   = null.hypothesis.variance,
               p.value     = p.value,
               null.value  = null.hypothesis.variance,
               alternative = alternative[1],
               method      = "One-Sample Chi-Square Test For Variance",
               conf.int    = c(df*v/chiupper,df*v/chilower)
  )
  
  names(retval$statistic) <- "chi-square statistic"
  names(retval$null.value) <- "variance"
  names(retval$parameter) <- "null hypothesis variance"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}