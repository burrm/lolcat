mean.z.test.twosample.independent.simple<-function(
  sample.mean.g1
  ,variance.g1
  ,sample.size.g1
  ,sample.mean.g2 = sample.mean.g1
  ,variance.g2 = variance.g1
  ,sample.size.g2 = sample.size.g1
  ,null.hypothesis.difference = 0
  ,alternative = c("two.sided","less","greater") #difference in means only
  ,conf.level = 0.95
  ,g1.details = T #Include point estimates and confidence intervals for mean
  ,g2.details = T #Include point estimates and confidence intervals for mean
  #,details.include = c("mean", "mean.ci", "var") #,"var.ci" ,"sd", "sd.ci"
) {
  validate.htest.alternative(alternative = alternative)
  
  
  #Independent samples, equal variances
  s.denom <- sqrt(variance.g1/sample.size.g1+variance.g2/sample.size.g2)
  z       <- ((sample.mean.g1 - sample.mean.g2)-null.hypothesis.difference)/s.denom
  
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  diff.upper <- (sample.mean.g1 - sample.mean.g2) + cv*s.denom
  diff.lower <- (sample.mean.g1 - sample.mean.g2) - cv*s.denom
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pnorm(z)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pnorm(z, lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pnorm(z, lower.tail = TRUE)
  } else {
    NA
  }
  
  estimate <- c(diff = sample.mean.g1-sample.mean.g2
                ,se.est = s.denom
  )
  
  if (g1.details) {
    g1.z.out <- mean.z.test.onesample.simple(sample.mean = sample.mean.g1,
                                             known.population.variance = variance.g1,
                                        sample.size = sample.size.g1,
                                        conf.level = conf.level)
    # g1.chi.out <- variance.test.onesample.simple(sample.variance = sample.variance.g1,
    #                                              sample.size = sample.size.g1, 
    #                                              conf.level = var.test.conf.level)
    # 
    estimate<-c(estimate
                ,g1.mean = sample.mean.g1
                ,g1.mean.lowerci = g1.z.out$conf.int[1]
                ,g1.mean.upperci = g1.z.out$conf.int[2]
                ,g1.sample.size = sample.size.g1
                # ,g1.var = sample.variance.g1
                # ,g1.var.lowerci = g1.chi.out$conf.int[1]
                # ,g1.var.upperci = g1.chi.out$conf.int[2]
                # ,g1.sd = sqrt(sample.variance.g1)
                # ,g1.sd.lowerci = sqrt(g1.chi.out$conf.int[1])
                # ,g1.sd.upperci = sqrt(g1.chi.out$conf.int[2])
    )
  }
  
  if (g2.details) {
    g2.z.out <- mean.z.test.onesample.simple(sample.mean = sample.mean.g2,
                                        known.population.variance = variance.g2,
                                        sample.size = sample.size.g2,
                                        conf.level = conf.level)
    # g2.chi.out <- variance.test.onesample.simple(sample.variance = sample.variance.g2,
    #                                              sample.size = sample.size.g2, 
    #                                              conf.level = var.test.conf.level)
    
    estimate<-c(estimate
                ,g2.mean = sample.mean.g2
                ,g2.mean.lowerci = g2.z.out$conf.int[1]
                ,g2.mean.upperci = g2.z.out$conf.int[2]
                ,g2.sample.size = sample.size.g2
                # ,g2.var = sample.variance.g2
                # ,g2.var.lowerci = g2.chi.out$conf.int[1]
                # ,g2.var.upperci = g2.chi.out$conf.int[2]
                # ,g2.sd = sqrt(sample.variance.g2)
                # ,g2.sd.lowerci = sqrt(g2.chi.out$conf.int[1])
                # ,g2.sd.upperci = sqrt(g2.chi.out$conf.int[2])
    )
  }
  
  # if (var.test.details) {
  #   estimate<-c(estimate
  #               ,var.test.conf.level = var.test.conf.level
  #               ,var.test.F = rmnames(var.equality.test.out$statistic)
  #               ,var.test.df.g1 = sample.size.g1 - 1
  #               ,var.test.df.g2 = sample.size.g2 - 1
  #               ,var.test.p = var.equality.test.out$p.value
  #   )
  # }
  
  retval<-list(data.name   = "input sample means and known variances",
               statistic   = z, 
               estimate    = estimate,
               parameter   = null.hypothesis.difference,
               p.value     = p.value,
               null.value  = null.hypothesis.difference,
               alternative = alternative[1],
               method      = paste("Two-Sample z Test For Difference in Means (Known Population Variances)"),
               conf.int    = c(diff.lower, diff.upper)
  )
  
  #names(retval$estimate) <- c("sample mean", "df")
  names(retval$statistic) <- "z statistic"
  names(retval$null.value) <- "difference of means"
  names(retval$parameter) <- "null hypothesis difference"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}


#t.test.twosample.independent.simple(sample.mean.g1 = 2,sample.variance.g1 = 1,sample.size.g1 = 10,sample.mean.g2 = 3,sample.variance.g2 = 1,sample.size.g2 = 10)
