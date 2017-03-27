proportion.test.twosample.approximate.simple <- function(
  sample.proportion.g1 
  ,sample.size.g1
  ,sample.proportion.g2 
  ,sample.size.g2
  
  ,alternative = c("two.sided", "less", "greater")
  ,conf.level = .95
  ,continuity.correction = T
) {
  validate.htest.alternative(alternative = alternative)
  
  p1.test <- proportion.test.onesample.approximate.simple(sample.proportion = sample.proportion.g1
                                                          ,sample.size = sample.size.g1
                                                          ,null.hypothesis.proportion = .5
                                                          ,alternative = alternative
                                                          ,conf.level = conf.level
                                                          ,continuity.correction = continuity.correction
                                                          )

  p2.test <- proportion.test.onesample.approximate.simple(sample.proportion = sample.proportion.g2
                                                          ,sample.size = sample.size.g2
                                                          ,null.hypothesis.proportion = .5
                                                          ,alternative = alternative
                                                          ,conf.level = conf.level
                                                          ,continuity.correction = continuity.correction
  )
  
    
  d <- sample.proportion.g1 - sample.proportion.g2
  np1 <- sample.proportion.g1 * sample.size.g1
  np2 <- sample.proportion.g2 * sample.size.g2
  
  p.hat <- (np1+np2) / (sample.size.g1 + sample.size.g2)
  
  
  if (continuity.correction) {
    if (alternative[1] == "two.sided") {
      d <- d + sign(sample.proportion.g2-sample.proportion.g1)*(1/(2*sample.size.g1) + 1/(2*sample.size.g2))
    } else if (alternative[1] == "greater") {
      d <- d - (1/(2*sample.size.g1) + 1/(2*sample.size.g2))
    } else if (alternative[1] == "less") {
      d <- d + (1/(2*sample.size.g1) + 1/(2*sample.size.g2))
    }
  }
  
  se.est <- sqrt(p.hat*(1-p.hat)*(1/sample.size.g1 + 1/sample.size.g2))
  
  z <- d/se.est
  
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  
  z.upper <- d + cv*se.est
  z.lower <- d - cv*se.est
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pnorm(z)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pnorm(z,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pnorm(z,lower.tail = TRUE)
  } else {
    NA
  }
  
  
  retval<-list(data.name   = "sample proportions and sample sizes",
               statistic   = c(z=z), 
               estimate    = c(diff = d 
                               ,p.hat = p.hat
                               ,se.est = se.est
                               ,sample.prop.g1 = sample.proportion.g1
                               ,sample.size.g1 = sample.size.g1
                               ,n1.times.p1 = np1
                               ,p1.lowerci = p1.test$conf.int[1]
                               ,p1.upperci = p1.test$conf.int[2]
                               ,sample.prop.g2 = sample.proportion.g2
                               ,sample.size.g2 = sample.size.g2
                               ,n2.times.p2 = np2
                               ,p2.lowerci = p2.test$conf.int[1]
                               ,p2.upperci = p2.test$conf.int[2]
                               
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Two-Sample Independent Proportion Test (Approximate)",
               conf.int    = c(z.lower,z.upper)
  )
  
  #names(retval$estimate) <- c("sample proportion")
  names(retval$null.value) <- "proportion difference"
  names(retval$parameter) <- "null hypothesis proportion difference"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}


#proportion.test.onesample.approximate.simple(sample.proportion = .8, sample.size = 25, null.hypothesis.proportion = .5, continuity.correction = F)
# proportion.test.twosample.approximate.simple(
#   sample.proportion.g1 = .2
#   ,sample.size.g1 = 20
#   ,sample.proportion.g2 = .5
#   ,sample.size.g2 = 50
#   ,alternative = "greater"
# )
