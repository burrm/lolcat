power.mean.z.onesample <- function(sample.size
                                   ,effect.size
                                   ,se.est = 1
                                   ,alpha = .05
                                   ,alternative = c("two.sided","less","greater")
                                   ,details = T
) {
  
  ncp <- effect.size
  z_alpha <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
  z_alpha <- z_alpha*se.est/sqrt(sample.size)
  
  pow <- pnorm(z_alpha,mean=ncp,sd=se.est/sqrt(sample.size),lower.tail = F)
  
  if (details) {
    as.data.frame(list(test="z"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = sample.size
                       ,actual = sample.size
                       ,effect.size = effect.size
                       ,se.est = se.est
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = 1-pow
                       ,power = pow
    ))
    
  }
  else {
    pow
  }
  
  
  
}

#power.mean.z.onesample(sample.size = 4, effect.size = .5, se.est = 1)
