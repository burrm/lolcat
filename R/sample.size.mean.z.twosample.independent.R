sample.size.mean.z.twosample.independent <- function(effect.size
                                         ,variance = 1
                                         ,alpha = .05
                                         ,beta = .1
                                         ,alternative = c("two.sided","less","greater")
                                         ,details = TRUE
                                         ,power.from.actual = F #report parameter power instead of true power
) {
  validate.htest.alternative(alternative = alternative)
  se.est <- sqrt(variance)
  
  size.1s <- sample.size.mean.z.onesample(effect.size = effect.size
                                          ,variance = variance
                                          ,alpha = alpha
                                          ,beta = beta
                                          ,alternative = alternative
                                          ,details = T
                                          ,power.from.actual = power.from.actual
                                          )
  
  n <- size.1s$actual * 2
  n.rounded <- ceiling(n)
  
  if (power.from.actual) {
    
  } else {
    z_alpha <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
    z_delta <- effect.size/(se.est/sqrt(n.rounded/2))
    
    beta <- pnorm(z_alpha
                  , lower.tail = T
                  , mean = z_delta
                  )
  }
  
  
  
  if (details) {
    as.data.frame(list(test="z"
                       ,type = "two.sample"
                       ,alternative = alternative[1]
                       ,sample.size = n.rounded
                       ,actual = n
                       ,effect.size = effect.size
                       ,variance = variance
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = beta
                       ,power = 1- beta
    ))
    
  }
  else {
    n.rounded
  }
  
}