cor.pearson.r.twosample.independent.simple <- function(
  sample.r.g1.g2,
  sample.size.g1.g2,
  sample.r.g3.g4,
  sample.size.g3.g4,
  alternative = c("two.sided","less","greater"),
  conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  r1 <- sample.r.g1.g2
  n1 <- sample.size.g1.g2
  r2 <- sample.r.g3.g4
  n2 <- sample.size.g3.g4
  
  r1.test <- cor.pearson.r.onesample.simple(sample.r = r1, sample.size = n1, conf.level = conf.level)
  r2.test <- cor.pearson.r.onesample.simple(sample.r = r2, sample.size = n2, conf.level = conf.level)
  
  z_r1 <- .5*log((1+r1)/(1-r1))
  z_r2 <- .5*log((1+r2)/(1-r2))
  
    
    z <- (z_r1-z_r2)/sqrt(1/(n1-3) + 1/(n2-3))
    
    estimate = c(r_12 = r1,
                 n_12 = n1,
                 z_r12 = z_r1,
                 r_12_lowerci = r1.test$conf.int[1],
                 r_12_upperci = r1.test$conf.int[2],
                 r_12.squared = r1^2,
                 
                 r_34 = r2,
                 n_34 = n2,
                 z_r34 = z_r2,
                 r_34_lowerci = r2.test$conf.int[1],
                 r_34_upperci = r2.test$conf.int[2],
                 r_34.squared = r2^2
                 
                 
                 )
    
    statistic <- c(z.statistic = z)
    
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
    
  
  
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  
  z_r.lowerci <- (z_r1-z_r2) - cv*sqrt(1/(n1-3) + 1/(n2-3))
  z_r.upperci <- (z_r1-z_r2) + cv*sqrt(1/(n1-3) + 1/(n2-3))
  

  retval<-list(data.name   = "sample correlations and sample sizes",
               statistic   = statistic, 
               estimate    = estimate,
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Two-Sample Independent Test for Pearson Product Moment Correlation",
               conf.int    = c(z_r.lowerci,z_r.upperci)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "correlation difference"
  names(retval$parameter) <- "null hypothesis correlation difference"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}
