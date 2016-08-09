sample.size.mean.t.onesample <- function(effect.size
                                         ,variance.est = 1
                                         ,alpha = .05
                                         ,beta = .1
                                         ,alternative = c("two.sided","less","greater")
                                         ,details = TRUE
                                         ,power.from.actual = F #report parameter power instead of true power
) {
  se.est <- sqrt(variance.est)
  
  z.res <- sample.size.mean.z.onesample(effect.size = effect.size
                                        ,variance = variance.est
                                        ,alpha = alpha
                                        ,beta = beta
                                        ,alternative = alternative
                                        ,details = T
                                        ,power.from.actual = power.from.actual
  )
  
  df <- max(z.res$sample.size-1, 1)
  
  n_gen <- function(df) {
    t_alpha <- qt(ifelse(alternative[1] == "two.sided",alpha/2,alpha), df= df, lower.tail = F)
    t_beta  <- qt(beta, df = df, lower.tail = F)

    ((t_alpha + t_beta)*se.est/effect.size)^2    
  }
  
  n <- z.res$sample.size
  
  while (n != ceiling(n_gen(df))) {
    df <- df+1
    n <- n+1
  }
    
  actual <- n_gen(df) #-> misleading?

  if (effect.size > 0 && alternative == "less") {
    df <- NA
    n <- NA
    actual <- NA
  }

  if (effect.size < 0 && alternative == "greater") {
    df <- NA
    n <- NA
    actual <- NA
  }
  
    
  if (power.from.actual) {
    power <- 1- beta
  } else {
    power <- power.mean.t.onesample(sample.size = n, 
                                    effect.size = effect.size,
                                    variance = variance.est,
                                    alpha=alpha,
                                    alternative = alternative,
                                    details=FALSE
    )
    
    beta <- 1-power
    
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
