t.test.onesample.simple<-function(sample.mean
                                  ,sample.variance
                                  ,sample.size
                                  ,h0.mean = 0
                                  ,alternative = c("two.sided","less","greater")
                                  ,conf.level = 0.95
                                  ,finite.population.N = NA
) {
  se.est <- sqrt(sample.variance/sample.size)
  
  if (!is.na(finite.population.N)) {
    se.est <- se.est * sqrt((finite.population.N-sample.size)/(finite.population.N-1))
  }
  
  t       <- (sample.mean-h0.mean)/se.est
  df      <- sample.size-1 
  cv      <- qt(conf.level+(1-conf.level)/2, df= df)
  mean.upper <- sample.mean + cv*se.est
  mean.lower <- sample.mean - cv*se.est
  
  var.test.out <- variance.test.onesample.simple(sample.variance = sample.variance
                                                 ,sample.size = sample.size
                                                 ,h0.variance = 1
                                                 ,conf.level = conf.level)
  var.lower <- var.test.out$conf.int[1]
  var.upper <- var.test.out$conf.int[2]
  sd.lower<-sqrt(var.lower)
  sd.upper<-sqrt(var.upper)
  
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
  
  pow <- power.mean.t.onesample(sample.size = sample.size
                                ,effect.size = if (alternative[1] == "two.sided") {
                                  abs(sample.mean-h0.mean)
                                } else if (alternative[1] == "greater") {
                                  sample.mean - h0.mean
                                } else {
                                  h0.mean - sample.mean
                                }
                                
                                ,variance.est = sample.variance
                                ,alpha = 1-conf.level
                                ,alternative = alternative
                                ,details = F)
  
  
  
  retval<-list(data.name   = deparse(substitute(sample.mean)),
               statistic   = t, 
               estimate    = c(sample.mean = sample.mean
                               ,se.est = se.est
                               ,df = df
                               ,var.lowerci = var.lower
                               ,var.upperci = var.upper
                               ,sd.lowerci  = sd.lower
                               ,sd.upperci = sd.upper
                               ,power = pow),
               parameter   = h0.mean,
               p.value     = p.value,
               null.value  = h0.mean,
               alternative = alternative[1],
               method      = "One-Sample t Test For Means",
               conf.int    = c(mean.lower, mean.upper)
  )
  
  #names(retval$estimate) <- c("sample mean", "df")
  names(retval$statistic) <- "t statistic"
  names(retval$null.value) <- "mean"
  names(retval$parameter) <- "null hypothesis mean"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}

#require(lolcat)
#t.test.onesample.simple(sample.mean = .5,sample.variance = 1,sample.size = 25)

