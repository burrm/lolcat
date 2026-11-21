sample.size.cor.pearson.r.onesample <- function(
  null.hypothesis.correlation
  ,alternative.hypothesis.correlation
  ,alpha = .05
  ,beta = .1
  ,alternative = c("two.sided","less","greater")
  ,details = TRUE
  ,power.from.actual = F #report parameter power instead of true power
) {
  validate.htest.alternative(alternative = alternative)
  z_r.null <- .5*log((1+null.hypothesis.correlation)/(1-null.hypothesis.correlation))
  z_r.alternative <- .5*log((1+alternative.hypothesis.correlation)/(1-alternative.hypothesis.correlation))
  
  z_alpha <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
  z_beta  <- qnorm(beta, lower.tail = F)
  
  n <- ((z_alpha + z_beta) / (z_r.alternative - z_r.null))^2 +3
  
  if (alternative[1] == "greater" && z_r.alternative < z_r.null) {
    n <- NA
  }
  
  if (alternative[1] == "less" && z_r.alternative > z_r.null) {
    n <- NA
  }
  
  n.rounded <- ceiling(n)
  
  if (power.from.actual) {
    
    power <- 1-beta
    
  } else {
    
    #beta <- pnorm(sqrt(n.rounded) * effect.size/se.est  - qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
    #           , lower.tail = F)
    
    power <- power.cor.pearson.r.onesample(
      sample.size = n.rounded
      ,null.hypothesis.correlation = null.hypothesis.correlation
      ,alternative.hypothesis.correlation = alternative.hypothesis.correlation
      ,alpha=alpha
      ,alternative = alternative
      ,details=FALSE
    )
    
    beta <- 1-power
    
  }
  
  if (details) {
    as.data.frame(list(
      test="z"
      ,type = "cor.pearson.r.onesample"
      ,alternative = alternative[1]
      ,sample.size = n.rounded
      ,actual = n
      ,effect.size = alternative.hypothesis.correlation - null.hypothesis.correlation
      #,variance = 1
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


