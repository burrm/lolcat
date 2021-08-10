sample.size.count.poisson.onesample.approximate <- function(
   lambda.null.hypothesis
  ,lambda.alternative.hypothesis
  ,alpha = .05
  ,beta  = .1
  ,alternative = c("two.sided","less","greater")
  ,details = TRUE
  ,power.from.actual = F #report parameter power instead of true power
) {
  validate.htest.alternative(alternative = alternative)
  
  z_alpha <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
  z_beta  <- qnorm(beta, lower.tail = F)
  
  n <- (1/4)*((z_alpha + z_beta) / (sqrt(lambda.alternative.hypothesis)-sqrt(lambda.null.hypothesis)))^2 
  
  if (alternative[1] == "greater" && lambda.alternative.hypothesis < lambda.null.hypothesis) {
    n <- NA
  }
  
  if (alternative[1] == "less" && lambda.alternative.hypothesis > lambda.null.hypothesis) {
    n <- NA
  }
  
  n.rounded <- ceiling(n)
  
  if (power.from.actual) {
    
    power <- 1-beta
    
  } else {
      
    power <- power.count.poisson.onesample.approximate(
      sample.size                    = n.rounded 
      ,lambda.null.hypothesis        = lambda.null.hypothesis
      ,lambda.alternative.hypothesis = lambda.alternative.hypothesis
      ,alpha                         = alpha
      ,alternative                   = alternative
      ,details                       = FALSE
    )
    
    beta <- 1-power
    
  }
  
  if (details) {
  as.data.frame(list(test="poisson"
                     ,type = "one.sample"
                     ,alternative = alternative[1]
                     ,sample.size = n.rounded
                     ,actual = n
                     ,lambda.null = lambda.null.hypothesis
                     ,lambda.alternative = lambda.alternative.hypothesis
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


