#Mcnemar's test - chi^2 = (b-c)^2/(b+c)
#                 normal approx   (b-c)/sqrt(b+c)
#                 continuity correction  (|b-c| -1) /sqrt(b+c) - also applies chi-square


proportion.test.mcnemar.simple <- function(
  b
  ,c
  ,null.hypothesis.proportion = .5
  ,alternative = c("two.sided", "less", "greater")
  ,conf.level = .95
  ,method = c("Exact")#, "normal", "chi-square")
  #,continuity.correction = T
) {
  validate.htest.alternative(alternative = alternative)
  p_b <- b/(b+c)
  
  p.test.out <- proportion.test.onesample.exact.simple(sample.proportion = p_b
                                                       ,sample.size = b+c
                                                       ,null.hypothesis.proportion = null.hypothesis.proportion
                                                       ,alternative = alternative[1]
                                                       ,conf.level = conf.level
                                                       )
  

  retval<-list(data.name   = "off-diagonal 2x2 elements",
               statistic   = c(p = p_b), 
               estimate    = c(b = b 
                               ,p_b = p_b
                               ,c = c
                               ,p_c = c/ (b+c)

               ),
               parameter   = null.hypothesis.proportion,
               p.value     = p.test.out$p.value,
               null.value  = null.hypothesis.proportion,
               alternative = alternative[1],
               method      = paste("McNemar's Test for Dependent Proportions (", method[1] ,")",sep ="")
               #,conf.int    = c(lowerci,upperci)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "proportion"
  names(retval$parameter) <- "null hypothesis proportion"
  #attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}

#proportion.test.mcnemar.simple(5,7, alternative = "less")

#5/12
