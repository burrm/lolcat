sample.size.proportion.test.onesample.approximate <- function(null.hypothesis.proportion
                                                              ,alternative.hypothesis.proportion
                                                              ,alpha = .05
                                                              ,beta = .1
                                                              ,alternative = c("two.sided","less","greater")
                                                              ,details = TRUE
                                                              ,power.from.actual = F #report parameter power instead of true power
) {
  validate.htest.alternative(alternative = alternative)
  z_alpha <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
  z_beta  <- qnorm(beta, lower.tail = F)
  
  se.null        <- null.hypothesis.proportion*(1-null.hypothesis.proportion)
  se.alternative <- alternative.hypothesis.proportion*(1-alternative.hypothesis.proportion)
  
  
  n <- ((z_alpha*sqrt(se.null) + z_beta*sqrt(se.alternative)) / (null.hypothesis.proportion - alternative.hypothesis.proportion))^2 
  
  n.rounded <- ceiling(n)
  
  if (power.from.actual) {
    
    power <- 1-beta
    
  } else {
    
    #beta <- pnorm(sqrt(n.rounded) * effect.size/se.est  - qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
    #           , lower.tail = F)
    
    power <- power.proportion.test.onesample.approximate(sample.size = n.rounded, 
                                                         null.hypothesis.proportion = null.hypothesis.proportion,
                                                         alternative.hypothesis.proportion = alternative.hypothesis.proportion,
                                                         alpha=alpha,
                                                         alternative = alternative,
                                                         details=FALSE
    )
    
    beta <- 1-power
    
  }
  
  if (details) {
    as.data.frame(list(test="proportion"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = n.rounded
                       ,actual = n
                       ,null.hypothesis.proportion = null.hypothesis.proportion
                       ,alternative.hypothesis.proportion = alternative.hypothesis.proportion
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


