power.cor.pearson.r.onesample <- function(
  sample.size
  ,null.hypothesis.correlation
  ,alternative.hypothesis.correlation
  ,alpha = .05
  ,alternative = c("two.sided","less","greater")
  ,details = T
) {
  validate.htest.alternative(alternative = alternative)
  z_r.null <- .5*log((1+null.hypothesis.correlation)/(1-null.hypothesis.correlation))
  z_r.alternative <- .5*log((1+alternative.hypothesis.correlation)/(1-alternative.hypothesis.correlation))
  se <- sqrt(1/(sample.size-3))
  
  ncp <- (z_r.alternative - z_r.null)/se
  
  z.upper <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
  z.lower <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = T)   
  
  beta <- NA
  
  if (ncp < 0) {
    if (alternative[1] == "two.sided") {
      
      beta <- pnorm(z.lower
                    ,mean=ncp
                    ,sd=1
                    ,lower.tail = F)
      
    } else if (alternative[1] == "greater") {
      
      beta <- pnorm(z.upper
                    ,mean=ncp
                    ,sd=1
                    ,lower.tail = T)
      
      
    } else {
      
      beta <- pnorm(z.lower
                    ,mean=ncp
                    ,sd=1
                    ,lower.tail = F)
      
      
    }  
  } else if (ncp >= 0) {
    if (alternative[1] == "two.sided") {
      
      beta <- pnorm(z.upper
                    ,mean=ncp
                    ,sd=1
                    ,lower.tail = T)    
      
    } else if (alternative[1] == "greater") {
      
      beta <- pnorm(z.upper
                    ,mean=ncp
                    ,sd=1
                    ,lower.tail = T)
      
    } else {
      
      beta <- pnorm(z.lower
                    ,mean=ncp
                    ,sd=1
                    ,lower.tail = F)
      
    } 
  } 
  
  pow <- 1-beta
  
  if (details) {
    as.data.frame(list(test="z"
                       ,type = "cor.pearson.r.onesample"
                       ,alternative = alternative[1]
                       ,sample.size = sample.size
                       ,actual = sample.size
                       ,effect.size = alternative.hypothesis.correlation - null.hypothesis.correlation
                       #,variance = 1
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