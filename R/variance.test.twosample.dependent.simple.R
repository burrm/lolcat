variance.test.twosample.dependent.simple<-function(
  sample.variance.g1
  ,sample.variance.g2
  ,n
  ,rho.estimate #Pearson r estimate
  ,h0.difference = 0
  ,alternative = c("two-sided","less","greater")
  ,conf.level = 0.95
  #,auto.swap = T
) {
  # if (sample.variance.g2 > sample.variance.g1 & auto.swap) {
  #   tmp<-sample.variance.g1
  #   sample.variance.g1<-sample.variance.g2
  #   sample.variance.g2<-tmp
  # }
  # 
  # if (sample.variance.g2 > sample.variance.g1) { 
  #   warning("Group 1 Variance should be >= Group 2 Variance for dependent sample t test.") 
  # }
  
  df     <- n - 2
  se.est <- sqrt(4*sample.variance.g1*sample.variance.g2*(1-rho.estimate^2))/sqrt(n-2)
  t <- ((sample.variance.g1 - sample.variance.g2) - h0.difference)/se.est
  cv      <- qt(conf.level+(1-conf.level)/2, df= df)
  
  upperci <- ((sample.variance.g1 - sample.variance.g2) + cv * se.est) 
  lowerci <- ((sample.variance.g1 - sample.variance.g2) - cv * se.est) 
  
  
  p.value <- if (alternative[1] == "two-sided") {
    tmp<-pt(t,df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "less") {
    pt(t,df,lower.tail = FALSE)
  } else if (alternative[1] == "greater") {
    pt(t,df,lower.tail = TRUE)
  } else {
    NA
  }
  
  retval<-list(data.name   = "dependent sample variances, sample size, and r",
               statistic   = t, 
               estimate    = c(sample.variance.g1 = sample.variance.g1
                               ,sample.variance.g2 = sample.variance.g2
                               ,pearson.estimate = rho.estimate
                               ,sample.size = n
                               ,df = df),
               parameter   = h0.difference,
               p.value     = p.value,
               null.value  = h0.difference,
               alternative = alternative[1],
               method      = "Two Dependent Sample t Test For Variance"
               ,conf.int    = c(lowerci, upperci)
  )
  
  names(retval$statistic)  <- "t statistic"
  names(retval$null.value) <- "variance difference"
  names(retval$parameter)  <- "variance difference"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}