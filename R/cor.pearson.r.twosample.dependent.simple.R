cor.pearson.r.twosample.dependent.simple <- function(
  sample.r.g1.g3,
  sample.r.g2.g3,
  sample.r.g1.g2,
  sample.size,
  alternative = c("two.sided","less","greater"),
  method = c("Steiger", "Hotelling"),
  conf.level = .95
) {
  
  
  rxy <- sample.r.g1.g2
  ryz <- sample.r.g2.g3
  rxz <- sample.r.g1.g3
  n   <- sample.size
  
  df <- n-3
  
  rxy.test <- cor.pearson.r.onesample.simple(sample.r = rxy, sample.size = n, conf.level = conf.level)
  ryz.test <- cor.pearson.r.onesample.simple(sample.r = ryz, sample.size = n, conf.level = conf.level)
  rxz.test <- cor.pearson.r.onesample.simple(sample.r = rxz, sample.size = n, conf.level = conf.level)

  if (method[1] == "Steiger") {
    se.est <- sqrt((n-1)*(1+rxy) / ( 2*((n-1)/(n-3))*(1-ryz^2-rxz^2-rxy^2+2*ryz*rxz*rxy) + ((ryz+rxz)/2)^2 * (1-rxy)^3 ) )
    t <- (rxz-ryz) * se.est
      
  } else if (method[1] == "Hotelling") {
    se.est <- sqrt((n-3)*(1+rxy) / (2*(1-ryz^2-rxy^2-rxz^2 + 2*ryz*rxy*rxz)))
    t <- (rxz-ryz) * se.est
    
  }
  

  estimate = c(diff.13.23 = rxz-ryz,
               df = df,
               r_13 = rxz,
               r_13_lowerci = rxz.test$conf.int[1],
               r_13_upperci = rxz.test$conf.int[2],
               r_13.squared = rxz^2,
               r_23 = ryz,
               r_23_lowerci = ryz.test$conf.int[1],
               r_23_upperci = ryz.test$conf.int[2],
               r_23.squared = ryz^2,
               r_12 = rxy,
               r_12_lowerci = rxy.test$conf.int[1],
               r_12_upperci = rxy.test$conf.int[2],
               r_12.squared = rxy^2
               
               
  )
  
  statistic <- c(t.statistic = t)
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pt(t,df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pt(t,df, lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pt(t,df, lower.tail = TRUE)
  } else {
    NA
  }
  
  
  
  cv      <- qt(conf.level+(1-conf.level)/2, df)
  
  lowerci <- (rxz-ryz) - cv/se.est
  upperci <- (rxz-ryz) + cv/se.est
  
  
  retval<-list(data.name   = "sample correlations and sample size",
               statistic   = statistic, 
               estimate    = estimate,
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = paste("Two-Sample Dependent Test for Pearson Product Moment Correlation (",method[1],")",sep=""),
               conf.int    = c(lowerci,upperci)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "correlation difference"
  names(retval$parameter) <- "null hypothesis correlation difference"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}

#Example from Sheskin pg 1279
#cor.pearson.r.twosample.dependent.simple(.955,.52,.37,5)
#Example from MVPStats - different hypothesis
#cor.pearson.r.twosample.dependent.simple(.77,.44,.66,300, method="Hotelling")
#Example from MVPStats - to match
#cor.pearson.r.twosample.dependent.simple(.77,.66,.44,300, method="Hotelling")

