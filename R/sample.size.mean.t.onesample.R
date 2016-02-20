sample.size.mean.t.onesample <- function(effect.size
                                         ,se.est = 1
                                         ,alpha = .05
                                         ,beta = .1
                                         ,alternative = c("two-sided","less","greater")
                                         ,details = TRUE
                                         ,include.z = FALSE #Only valid with details = T
                                         ,power.from.actual = F #report parameter power instead of true power
) {

  z.res <- sample.size.mean.z.onesample(effect.size = effect.size
                                        ,se.est = se.est
                                        ,alpha = alpha
                                        ,beta = beta
                                        ,alternative = alternative
                                        ,details = T
                                        ,power.from.actual = power.from.actual
  )
  
  df <- max(z.res$sample.size-1, 1)
  
  n_gen <- function(df) {
    t_alpha <- qt(ifelse(alternative[1] == "two-sided",alpha/2,alpha), df= df, lower.tail = F)
    t_beta  <- qt(beta, df = df, lower.tail = F)

    ((t_alpha + t_beta)*se.est/effect.size)^2    
  }
  
  n <- z.res$sample.size
  
  while (n != ceiling(n_gen(df))) {
    df <- df+1
    n <- n+1
  }
  
  actual <- n_gen(df) #-> misleading?

  if (power.from.actual) {
    
  } else {
    beta <- pt(sqrt(n) * effect.size/se.est  - qt(ifelse(alternative[1] == "two-sided",alpha/2,alpha), df= df, lower.tail = F)
               ,df = df
               ,lower.tail = F)
  }
  
  if (details) {
    ret <- as.data.frame(list(test="t"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = n
                       ,actual = actual
                       ,df = n-1
                       ,effect.size = effect.size
                       ,se.est = se.est
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = beta
                       ,power = 1- beta
    ))
    
    if (include.z) {
      z.res$df <- c(NA)
      ret <-rbind(ret,z.res)
      ret<- ret[2:1,]
      rownames(ret) <- 1:2
    }
    

        
    ret
  }
  else {
    n
  }
}

