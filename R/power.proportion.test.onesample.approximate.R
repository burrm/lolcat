power.proportion.test.onesample.approximate <- function(sample.size
                                                   ,null.hypothesis.proportion
                                                   ,alternative.hypothesis.proportion
                                                   ,alpha = .05
                                                   ,alternative = c("two.sided","less","greater")
                                                   ,details = T
) {
  validate.htest.alternative(alternative = alternative)
  
  n <- sample.size
  
  z_alpha <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
  
  se.null        <- null.hypothesis.proportion*(1-null.hypothesis.proportion)
  se.alternative <- alternative.hypothesis.proportion*(1-alternative.hypothesis.proportion)
  
  z_beta <- sqrt(n)*abs(null.hypothesis.proportion - alternative.hypothesis.proportion) - z_alpha*sqrt(se.null)
  z_beta <- z_beta / sqrt(se.alternative)
  
  pow <- pnorm(z_beta, lower.tail = T)
  
  if (alternative[1] == "less" & alternative.hypothesis.proportion > null.hypothesis.proportion) {
    pow <- NA
  } else if (alternative[1] == "greater" & alternative.hypothesis.proportion < null.hypothesis.proportion) {
    pow <- NA
  }
  
  if (details) {
    as.data.frame(list(test="proportion"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = sample.size
                       ,actual = sample.size
                       ,null.hypothesis.proportion = null.hypothesis.proportion
                       ,alternative.hypothesis.proportion = alternative.hypothesis.proportion
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