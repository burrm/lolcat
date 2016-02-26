power.mean.t.onesample <- function(sample.size
                                   ,effect.size
                                   ,se.est = 1
                                   ,alpha = .05
                                   ,alternative = c("two.sided","less","greater")
                                   ,details = T
) {
  
  
  ncp <- effect.size/(se.est/sqrt(sample.size))

  t_alpha <- qt(ifelse(alternative[1] == "two.sided",alpha/2,alpha), df = sample.size-1, lower.tail = F)
  
  pow <- pt(t_alpha,df = sample.size-1, ncp=ncp ,lower.tail = F)
  
  print(paste("ncp",ncp,"t_alpha",t_alpha,"pow",pow))
  
  
  if (details) {
    as.data.frame(list(test="t"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = sample.size
                       ,actual = sample.size
                       ,df = sample.size - 1
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

#power.mean.t.onesample(sample.size = 25, effect.size = .5, se.est = 1)
