poisson.test.twosample.simple<-function(
  sample.count.g1
  ,n.g1
  ,sample.count.g2
  ,n.g2
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95
) {
  
  lambda.hat<- (sample.count.g1 + sample.count.g2)/(n.g1+n.g2)
  u1<-sample.count.g1/n.g1
  u2<-sample.count.g2/n.g2
  
  z<-sign(u1-u2)*sqrt((sample.count.g1-n.g1*lambda.hat)^2/(n.g1*lambda.hat) + (sample.count.g2-n.g2*lambda.hat)^2/(n.g2*lambda.hat))
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pnorm(z)
    tmp<-min(tmp, 1-tmp)
    tmp*2
  } else if (alternative[1] == "greater") {
    pnorm(z , lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pnorm(z ,lower.tail = TRUE)
  } else {
    NA
  }
  
  g1.onesample.test <- poisson.test(sample.count.g1
                                    ,T = n.g1
                                    ,conf.level = conf.level)
  g2.onesample.test <- poisson.test(sample.count.g2
                                    ,T = n.g2
                                    ,conf.level = conf.level)
  

  retval<-list(data.name   = "sample counts and sample sizes",
               statistic   = z,
               estimate    = c(rate.g1 = u1
                               ,rate.g2 = u2
                               ,lambda.hat = lambda.hat
                               ,g1.lambda.lowerci = g1.onesample.test$conf.int[1]
                               ,g1.lambda.upperci = g1.onesample.test$conf.int[2]
                               ,g2.lambda.lowerci = g2.onesample.test$conf.int[1]
                               ,g2.lambda.upperci = g2.onesample.test$conf.int[2]
                             ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Two-Sample Poisson Test",
               conf.int    = c(NA,NA)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$statistic) <- "z"
  names(retval$null.value) <- "difference in rates"
  names(retval$parameter) <- "difference in rates"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}
