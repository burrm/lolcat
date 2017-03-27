sample.size.mean.t.onesample <- function(effect.size
                                         ,variance.est = 1
                                         ,alpha = .05
                                         ,beta = .1
                                         ,alternative = c("two.sided","less","greater")
                                         ,details = TRUE
                                         ,power.from.actual = F #report parameter power instead of true power
                                         ,max.iter = 1000000
) {
  validate.htest.alternative(alternative = alternative)
  
  se.est <- sqrt(variance.est)
  
  df <- 1
  
  
  if (effect.size > 0 && alternative == "less") {
    df <- NA
    n <- NA
    actual <- NA
    power <- NA
  } else if (effect.size < 0 && alternative == "greater") {
    df <- NA
    n <- NA
    actual <- NA
    power <- NA
  } else {
    target.power <- 1-beta
    
    power <- power.mean.t.onesample(sample.size = df + 1, 
                                    effect.size = effect.size,
                                    variance = variance.est,
                                    alpha=alpha,
                                    alternative = alternative,
                                    details=FALSE
    )
    n <- df +1
    actual <- n
    
        
    while (power < target.power & df <= max.iter) {
      df <- df+1
      n <- df +1
      actual <- n

      power <- power.mean.t.onesample(sample.size = n, 
                                      effect.size = effect.size,
                                      variance = variance.est,
                                      alpha=alpha,
                                      alternative = alternative,
                                      details=FALSE
      )
      
      
    }
    
    
  }
  
    
  if (power.from.actual) {
    power <- 1- beta
  } else {
    
  }
  
  if (details) {
    ret <- as.data.frame(list(test="t"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = n
                       ,actual = actual
                       ,df = n-1
                       ,effect.size = effect.size
                       ,variance = variance.est
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = beta
                       ,power = power
    ))
    
    # if (include.z) {
    #   z.res$df <- c(NA)
    #   ret <-rbind(ret,z.res)
    #   ret<- ret[2:1,]
    #   rownames(ret) <- 1:2
    # }
    

        
    ret
  }
  else {
    n
  }
}

#sample.size.mean.t.onesample(effect.size = 2,se.est = 1)
