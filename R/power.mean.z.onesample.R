power.mean.z.onesample <- function(sample.size
                                   ,effect.size
                                   ,variance = 1
                                   ,alpha = .05
                                   ,alternative = c("two.sided","less","greater")
                                   ,details = T
) {
  se.est <- sqrt(variance)
  
  z.upper <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
  z.lower <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = T)   
  
  z.upper <- z.upper*se.est/sqrt(sample.size)
  z.lower <- z.lower*se.est/sqrt(sample.size)
  
  beta <- NA
  
  if (effect.size < 0) {
    if (alternative[1] == "two.sided") {
      
      beta <- pnorm(z.lower
                    ,mean=effect.size
                    ,sd=se.est/sqrt(sample.size)
                    ,lower.tail = F)
      
    } else if (alternative[1] == "greater") {

      beta <- pnorm(z.upper
                    ,mean=effect.size
                    ,sd=se.est/sqrt(sample.size)
                    ,lower.tail = T)
      
            
    } else {
      
      beta <- pnorm(z.lower
                    ,mean=effect.size
                    ,sd=se.est/sqrt(sample.size)
                    ,lower.tail = F)
      
      
    }  
  } else if (effect.size >= 0) {
    if (alternative[1] == "two.sided") {
      
      beta <- pnorm(z.upper
                    ,mean=effect.size
                    ,sd=se.est/sqrt(sample.size)
                    ,lower.tail = T)    
      
    } else if (alternative[1] == "greater") {
      
      beta <- pnorm(z.upper
                    ,mean=effect.size
                    ,sd=se.est/sqrt(sample.size)
                    ,lower.tail = T)
      
    } else {
      
      beta <- pnorm(z.lower
                    ,mean=effect.size
                    ,sd=se.est/sqrt(sample.size)
                    ,lower.tail = F)
      
    } 
  } 
  
  pow <- 1-beta
  
  if (details) {
    as.data.frame(list(test="z"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = sample.size
                       ,actual = sample.size
                       ,effect.size = effect.size
                       ,variance = variance
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

#power.mean.z.onesample(sample.size = 4, effect.size = .5, variance =  1)
