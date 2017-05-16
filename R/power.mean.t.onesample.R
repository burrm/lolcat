power.mean.t.onesample <- function(sample.size
                                   ,effect.size
                                   ,variance.est = 1
                                   ,alpha = .05
                                   ,alternative = c("two.sided","less","greater")
                                   ,details = T
) {
  validate.htest.alternative(alternative = alternative)
  se.est <- sqrt(variance.est)
  
  df <- sample.size-1
  ncp <- effect.size/(se.est/sqrt(sample.size))
  
  t.upper <- qt(ifelse(alternative[1] == "two.sided",alpha/2,alpha), df = sample.size-1, lower.tail = F)
  t.lower <- qt(ifelse(alternative[1] == "two.sided",alpha/2,alpha), df = sample.size-1, lower.tail = T)
  
  beta <- NA
  
  if (effect.size < 0) {
    if (alternative[1] == "two.sided") {

      beta <- pt(t.lower
                 ,df = df
                 ,ncp=ncp
                 ,lower.tail = F)
      
            
    } else if (alternative[1] == "greater") {

      beta <- pt(t.upper
                 ,df = df
                 ,ncp=ncp
                 ,lower.tail = T)
      
    } else {

      beta <- pt(t.lower
                 ,df = df
                 ,ncp=ncp
                 ,lower.tail = F)
            
    }
    
  } else if (effect.size >= 0) {
    if (alternative[1] == "two.sided") {
     
      beta <- pt(t.upper
                ,df = df
                ,ncp=ncp
                ,lower.tail = T)
      
       
    } else if (alternative[1] == "greater") {
      
      beta <- pt(t.upper
                 ,df = df
                 ,ncp=ncp
                 ,lower.tail = T)
      
    } else {

      beta <- pt(t.lower
                 ,df = df
                 ,ncp=ncp
                 ,lower.tail = F)
      
            
    }
  }
  
  
  
  
  pow <- 1-beta
  
  if (details) {
    as.data.frame(list(test="t"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = sample.size
                       ,actual = sample.size
                       ,df = df
                       ,effect.size = effect.size
                       ,variance = variance.est
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = beta
                       ,power = pow
    ))
    
  }
  else {
    pow
  }
  
  
  
}

#power.mean.t.onesample(sample.size = 25, effect.size = .5, se.est = 1)
