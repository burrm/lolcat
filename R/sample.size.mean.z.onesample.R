sample.size.mean.z.onesample <- function(effect.size
                                         ,se.est = 1
                                         ,alpha = .05
                                         ,beta = .1
                                         ,alternative = c("two-sided","less","greater")
                                         ,details = TRUE
                                         ,power.from.actual = F #report parameter power instead of true power
                                         ) {
  z_alpha <- qnorm(ifelse(alternative[1] == "two-sided",alpha/2,alpha), lower.tail = F)
  z_beta  <- qnorm(beta, lower.tail = F)
  
  n <- ((z_alpha + z_beta) * se.est / effect.size)^2 
  
  n.rounded <- ceiling(n)
  
  if (power.from.actual) {
    
  } else {
  
    beta <- pnorm(sqrt(n.rounded) * effect.size/se.est  - qnorm(ifelse(alternative[1] == "two-sided",alpha/2,alpha), lower.tail = F)
               , lower.tail = F)
  }
  
  if (details) {
  as.data.frame(list(test="z"
                     ,type = "one.sample"
                     ,alternative = alternative[1]
                     ,sample.size = n.rounded
                     ,actual = n
                     ,effect.size = effect.size
                     ,se.est = se.est
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