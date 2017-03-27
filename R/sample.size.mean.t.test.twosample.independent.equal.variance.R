sample.size.mean.t.test.twosample.independent.equal.variance <- function(
  mean.g1 = 0
  ,mean.g2 = 1
  ,variance.est.g1 = 1
  ,variance.est.g2 = variance.est.g1
  ,alpha = .05
  ,beta = .1
  ,null.hypothesis.difference = 0
  ,alternative = c("two.sided","less","greater")
  ,details = TRUE
  ,power.from.actual = F #report parameter power instead of true power
) {
  validate.htest.alternative(alternative = alternative)
  
  sample.size.g1 <- 2
  sample.size.g2 <- 2

  current.power <- power.mean.t.test.twosample.independent.equal.variance(
    sample.size.g1 = sample.size.g1
    ,sample.size.g2 = sample.size.g2
    ,mean.g1 = mean.g1
    ,mean.g2 = mean.g2
    ,variance.est.g1 = variance.est.g1
    ,variance.est.g2 = variance.est.g2
    ,null.hypothesis.difference = null.hypothesis.difference
    ,alpha = alpha
    ,alternative = alternative
    ,details = T
  )
  
  beta.current <- current.power$beta[1]
  df           <- current.power$df[1]
#  se.est           <- current.power$se.est[1]
  
  while (beta.current > beta && !is.na(beta.current)) {
    sample.size.g1 <- sample.size.g1 + 1
    sample.size.g2 <- sample.size.g2 + 1
    
    current.power <- power.mean.t.test.twosample.independent.equal.variance(
      sample.size.g1 = sample.size.g1
      ,sample.size.g2 = sample.size.g2
      ,mean.g1 = mean.g1
      ,mean.g2 = mean.g2
      ,variance.est.g1 = variance.est.g1
      ,variance.est.g2 = variance.est.g2
      ,null.hypothesis.difference = null.hypothesis.difference
      ,alpha = alpha
      ,alternative = alternative
      ,details = T
    )
    
    beta.current <- current.power$beta[1]
    df           <- current.power$df[1]
#    se.est           <- current.power$se.est[1]
    
  }
  
  if (is.na(beta.current)) {
    sample.size.g1 <- NA
    sample.size.g2 <- NA

    beta.current <- NA
    df           <- NA
 #   se.est       <- NA
    
  }
  
  if (power.from.actual) {
    
  } else {
    beta <- beta.current
  }
  
  if (details) {
    as.data.frame(list(test="t"
                       ,type = "two.sample"
                       ,alternative = alternative[1]
                       ,mean.diff = (mean.g2-mean.g1)-null.hypothesis.difference
                       ,sample.size.g1 = sample.size.g1
                       ,sample.size.g2 = sample.size.g2
                       ,df = df
#                       ,se.est = se.est
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = beta
                       ,power = 1- beta
    ))
    
  }
  else {
    c(sample.size.g1=sample.size.g1,sample.size.g2=sample.size.g2)
  }
  
}

#sample.size.mean.t.test.twosample.independent.equal.variance(variance.est.g1 = 1, variance.est.g2 = 3)
#sample.size.mean.t.test.twosample.independent.unequal.variance(variance.est.g1 = 1, variance.est.g2 = 3)
