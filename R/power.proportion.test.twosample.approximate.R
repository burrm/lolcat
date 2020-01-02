power.proportion.test.twosample.approximate <- function(sample.size
                                                        ,proportion.g1
                                                        ,proportion.g2
                                                        ,alpha = .05
                                                        ,alternative = c("two.sided","less","greater")
                                                        ,details = T
) {
  validate.htest.alternative(alternative = alternative)
  
  n <- sample.size
  
  z_alpha <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
  
  se.g1        <- proportion.g1*(1-proportion.g1)
  se.g2        <- proportion.g2*(1-proportion.g2)
  
  z_beta <- sqrt(n)*abs(proportion.g1 - proportion.g2) - z_alpha*sqrt(2*se.g1)
  z_beta <- z_beta / sqrt(se.g1 + se.g2)
  
  pow <- pnorm(z_beta, lower.tail = T)
  
  if (alternative[1] == "less" & proportion.g1 > proportion.g2) {
    pow <- NA
  } else if (alternative[1] == "greater" & proportion.g1 < proportion.g2) {
    pow <- NA
  }
  
  if (details) {
    as.data.frame(list(test="proportion"
                       ,type = "two.sample"
                       ,alternative = alternative[1]
                       ,sample.size = sample.size
                       ,actual = sample.size
                       ,proportion.g1 = proportion.g1
                       ,proportion.g2 = proportion.g2
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