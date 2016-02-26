t.test.twosample.independent.simple<-function(sample.mean.g1
                                              ,sample.variance.g1
                                              ,sample.size.g1
                                              ,sample.mean.g2
                                              ,sample.variance.g2
                                              ,sample.size.g2
                                              ,h0.difference = 0
                                              ,assume.equal.variances = c("auto", "yes", "no")
                                              #Variance test hypothesis is always v1 = v2
                                              ,alternative = c("two.sided","less","greater") #difference in means only
                                              ,conf.level = 0.95
                                              ,var.test.conf.level = NA #Optional - specify different variance rejection level for "auto" and conf level for variance CI estimates
                                              ,var.test.details = T #Include test on equality of variance
                                              ,g1.details = T #Include point estimates and confidence intervals for mean, variance, and sd
                                              ,g2.details = T #Include point estimates and confidence intervals for mean, variance, and sd
) {
  if (is.na(var.test.conf.level)) {var.test.conf.level <- conf.level}
  
  if (assume.equal.variances[1] == "auto" | var.test.details) {
    var.equality.test.out <- variance.test.twosample.independent.simple(
      sample.variance.g1,
      sample.size.g1,
      sample.variance.g2,
      sample.size.g2,
      conf.level = var.test.conf.level
    )
  }
  
  equal.var <- if (assume.equal.variances[1] == "yes") {
    TRUE
  } else if (assume.equal.variances[1] == "no" ) {
    FALSE  
  } else {
    !(var.equality.test.out$p.value < 1 - var.test.conf.level)
  }
  
  
  if (equal.var) {
    #Independent samples, equal variances
    sp      <- ((sample.size.g1-1) * sample.variance.g1 + (sample.size.g2-1) * sample.variance.g2)/(sample.size.g1+sample.size.g2-2)
    s.denom <- sqrt(sp/sample.size.g1+sp/sample.size.g2)
    t       <- ((sample.mean.g1 - sample.mean.g2)-h0.difference)/s.denom
    df      <- sample.size.g1+sample.size.g2-2
  } else {
    #Independent samples, unequal variances
    s.denom <- sqrt(sample.variance.g1/sample.size.g1+sample.variance.g2/sample.size.g2)
    t       <- ((sample.mean.g1 - sample.mean.g2)-h0.difference)/s.denom
    df      <- (sample.variance.g1 / sample.size.g1 + sample.variance.g2 / sample.size.g2)^2 / ((sample.variance.g1/sample.size.g1)^2/(sample.size.g1-1) + (sample.variance.g2/sample.size.g2)^2/(sample.size.g2-1))
  }
  
  
  cv      <- qt(conf.level+(1-conf.level)/2, df= df)
  diff.upper <- (sample.mean.g1 - sample.mean.g2) + cv*s.denom
  diff.lower <- (sample.mean.g1 - sample.mean.g2) - cv*s.denom
  
  # var.test.out <- variance.test.onesample.simple(sample.variance = sample.variance
  #                                                ,n = n
  #                                                ,h0.variance = 1
  #                                                ,conf.level = conf.level)
  # var.lower <- var.test.out$conf.int[1]
  # var.upper <- var.test.out$conf.int[2]
  # sd.lower<-sqrt(var.lower)
  # sd.upper<-sqrt(var.upper)
  # 
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pt(t, df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pt(t, df, lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pt(t, df, lower.tail = TRUE)
  } else {
    NA
  }
  
  estimate <- c(diff = sample.mean.g1-sample.mean.g2
                ,se.est = s.denom
                ,df = df
  )
  
  if (g1.details) {
    g1.t.out <- t.test.onesample.simple(sample.mean = sample.mean.g1,
                                        sample.variance = sample.variance.g1,
                                        sample.size = sample.size.g1,
                                        conf.level = conf.level)
    g1.chi.out <- variance.test.onesample.simple(sample.variance = sample.variance.g1,
                                                 sample.size = sample.size.g1, 
                                                 conf.level = var.test.conf.level)
    
    estimate<-c(estimate
                ,g1.mean = sample.mean.g1
                ,g1.mean.lowerci = g1.t.out$conf.int[1]
                ,g1.mean.upperci = g1.t.out$conf.int[2]
                ,g1.sample.size = sample.size.g1
                ,g1.var = sample.variance.g1
                ,g1.var.lowerci = g1.chi.out$conf.int[1]
                ,g1.var.upperci = g1.chi.out$conf.int[2]
                ,g1.sd = sqrt(sample.variance.g1)
                ,g1.sd.lowerci = sqrt(g1.chi.out$conf.int[1])
                ,g1.sd.upperci = sqrt(g1.chi.out$conf.int[2])
    )
  }
  
  if (g2.details) {
    g2.t.out <- t.test.onesample.simple(sample.mean = sample.mean.g2,
                                        sample.variance = sample.variance.g2,
                                        sample.size = sample.size.g2,
                                        conf.level = conf.level)
    g2.chi.out <- variance.test.onesample.simple(sample.variance = sample.variance.g2,
                                                 sample.size = sample.size.g2, 
                                                 conf.level = var.test.conf.level)
    
    estimate<-c(estimate
                ,g2.mean = sample.mean.g2
                ,g2.mean.lowerci = g2.t.out$conf.int[1]
                ,g2.mean.upperci = g2.t.out$conf.int[2]
                ,g2.sample.size = sample.size.g2
                ,g2.var = sample.variance.g2
                ,g2.var.lowerci = g2.chi.out$conf.int[1]
                ,g2.var.upperci = g2.chi.out$conf.int[2]
                ,g2.sd = sqrt(sample.variance.g2)
                ,g2.sd.lowerci = sqrt(g2.chi.out$conf.int[1])
                ,g2.sd.upperci = sqrt(g2.chi.out$conf.int[2])
    )
  }
  
  if (var.test.details) {
    estimate<-c(estimate
                ,var.test.conf.level = var.test.conf.level
                ,var.test.F = rmnames(var.equality.test.out$statistic)
                ,var.test.df.g1 = sample.size.g1 - 1
                ,var.test.df.g2 = sample.size.g2 - 1
                ,var.test.p = var.equality.test.out$p.value
    )
  }
  
  retval<-list(data.name   = "input sample means and variances",
               statistic   = t, 
               estimate    = estimate,
               parameter   = h0.difference,
               p.value     = p.value,
               null.value  = h0.difference,
               alternative = alternative[1],
               method      = paste("Two-Sample t Test For Means", 
                                   if (equal.var) {"(Equal Variances)"} else {"(Unequal Variances)"}),
               conf.int    = c(diff.lower, diff.upper)
  )
  
  #names(retval$estimate) <- c("sample mean", "df")
  names(retval$statistic) <- "t statistic"
  names(retval$null.value) <- "difference of means"
  names(retval$parameter) <- "null hypothesis difference"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}


#t.test.twosample.independent.simple(sample.mean.g1 = 2,sample.variance.g1 = 1,sample.size.g1 = 10,sample.mean.g2 = 3,sample.variance.g2 = 1,sample.size.g2 = 10)
