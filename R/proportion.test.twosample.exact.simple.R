proportion.test.twosample.exact.simple <- function(
  sample.proportion.g1 
  ,sample.size.g1
  ,sample.proportion.g2 
  ,sample.size.g2
  ,alternative = c("two.sided", "less", "greater")
  ,conf.level = .95
) {
  
  np.g1 <- sample.proportion.g1*sample.size.g1
  np.g2 <- sample.proportion.g2*sample.size.g2
  
  mat <- matrix(c(np.g1,sample.size.g1 - np.g1
                  ,np.g2,sample.size.g2 - np.g2), ncol = 2)
  
  fisher.out <- fisher.test(mat,alternative = alternative[1])
  
  g1.test <- proportion.test.onesample.exact.simple(sample.proportion = sample.proportion.g1
                                                    ,sample.size = sample.size.g1
                                                    ,h0.proportion = .5
                                                    ,alternative = alternative[1]
                                                    ,conf.level = conf.level)
  

  g2.test <- proportion.test.onesample.exact.simple(sample.proportion = sample.proportion.g2
                                                    ,sample.size = sample.size.g2
                                                    ,h0.proportion = .5
                                                    ,alternative = alternative[1]
                                                    ,conf.level = conf.level)
    
  
  
  retval<-list(data.name   = "sample proportions and sample sizes",
               statistic   = c(odds.ratio = rmnames(fisher.out$statistic)), 
               estimate    = c(sample.prop.g1 = sample.proportion.g1 
                               ,sample.size.g1 = sample.size.g1
                               ,n1.times.p1 = np.g1
                               ,n1.times.q1 = sample.size.g1 - np.g1
                               ,p.g1.lowerci = g1.test$conf.int[1]
                               ,p.g1.upperci = g1.test$conf.int[2]
                               
                               ,sample.prop.g2 = sample.proportion.g2 
                               ,sample.size.g2 = sample.size.g2
                               ,n2.times.p2 = np.g2
                               ,n2.times.q2 = sample.size.g2 - np.g2
                               ,p.g2.lowerci = g2.test$conf.int[1]
                               ,p.g2.upperci = g2.test$conf.int[2]
               ),
               parameter   = 1,
               p.value     = fisher.out$p.value,
               null.value  = 1,
               alternative = alternative[1],
               method      = "Two-Sample Proportion Test - Fisher Exact Test"
               ,conf.int    = c(fisher.out$conf.int[1],fisher.out$conf.int[2])
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "odds ratio"
  names(retval$parameter) <- "null hypothesis odds ratio"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}