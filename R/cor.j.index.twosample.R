cor.j.index.twosample<-function(x1
                                ,x2
                                #,h0.difference = 0
                                ,alternative = c("two.sided","less","greater") #difference in means only
                                ,conf.level = 0.95

) {

  j.x1.test    <- cor.j.index.onesample(x1, alternative = alternative, conf.level = conf.level)
  j.x1.se.est    <- rmnames(j.x1.test$estimate)
  j.x1         <- rmnames(j.x1.test$statistic)
  j.x1.p       <- j.x1.test$p.value
  j.x1.lowerci <- j.x1.test$conf.int[1]
  j.x1.upperci <- j.x1.test$conf.int[2]
  
  
  j.x2.test    <- cor.j.index.onesample(x2, alternative = alternative, conf.level = conf.level)
  j.x2.se.est    <- rmnames(j.x2.test$estimate)
  j.x2         <- rmnames(j.x2.test$statistic)
  j.x2.p       <- j.x2.test$p.value
  j.x2.lowerci <- j.x2.test$conf.int[1]
  j.x2.upperci <- j.x2.test$conf.int[2]
  
  j.diff <- j.x1 - j.x2
  j.diff.se.est <- sqrt(j.x1.se.est^2 + j.x2.se.est^2)
  
  z <- (j.x1 - j.x2) / j.diff.se.est
  
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  diff.upper <- j.diff + cv*j.diff.se.est
  diff.lower <- j.diff - cv*j.diff.se.est
  
  estimate <- c(j.diff         = j.diff
                ,j.diff.se.est = j.diff.se.est
                ,j.x1          = j.x1
  #              ,j.x1.p        = j.x1.p
                ,j.x1.se.est   = j.x1.se.est
                ,j.x1.lowerci  = j.x1.lowerci
                ,j.x1.upperci  = j.x1.upperci
                ,j.x2          = j.x2
                ,j.x2.se.est   = j.x2.se.est
  #              ,j.x2.p        = j.x2.p
                ,j.x2.lowerci  = j.x2.lowerci
                ,j.x2.upperci  = j.x2.upperci
                
  ) 
  
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
  
  
  
  retval<-list(data.name   = "test results for two predictive tests",
               statistic   = z, 
               estimate    = estimate,
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "J Index of Predictive Efficiency",
               conf.int    = c(diff.lower, diff.upper)
  )
  
  names(retval$statistic) <- "z statistic"
  names(retval$null.value) <- "difference of J index"
  names(retval$parameter) <- "null hypothesis difference"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}
