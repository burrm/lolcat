sample.size.proportion.test.twosample.approximate <- function(proportion.g1
                                                              ,proportion.g2
                                                              ,alpha = .05
                                                              ,beta = .1
                                                              ,alternative = c("two.sided","less","greater")
                                                              ,details = TRUE
                                                              ,power.from.actual = F #report parameter power instead of true power
) {
  validate.htest.alternative(alternative = alternative)
  z_alpha <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
  z_beta  <- qnorm(beta, lower.tail = F)
  
  se.g1        <- proportion.g1*(1-proportion.g1)
  se.g2        <- proportion.g2*(1-proportion.g2)
  
  
  n <- ((z_alpha*sqrt(2*se.g1) + z_beta*sqrt(se.g1 + se.g2)) / (proportion.g1 - proportion.g2))^2 
  
  n.rounded <- ceiling(n)
  
  if (power.from.actual) {
    
    power <- 1-beta
    
  } else {
    
    #beta <- pnorm(sqrt(n.rounded) * effect.size/se.est  - qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
    #           , lower.tail = F)
    
    power <- power.proportion.test.twosample.approximate(sample.size = n.rounded, 
                                                         proportion.g1 = proportion.g1,
                                                         proportion.g2 = proportion.g2,
                                                         alpha=alpha,
                                                         alternative = alternative,
                                                         details=FALSE
    )
    
    beta <- 1-power
    
  }
  
  if (details) {
    as.data.frame(list(test="proportion"
                       ,type = "two.sample"
                       ,alternative = alternative[1]
                       ,sample.size = n.rounded
                       ,actual = n
                       ,proportion.g1 = proportion.g1
                       ,proportion.g2 = proportion.g2
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = beta
                       ,power = power
    ))
    
  }
  else {
    n.rounded
  }
}


