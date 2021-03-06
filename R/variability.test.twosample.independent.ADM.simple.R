variability.test.twosample.independent.ADM.simple<-function(
  sample.mean.ADM.g1 = 0
  ,sample.variance.ADM.g1 = 1
  ,sample.size.g1 = 10
  ,sample.mean.ADM.g2 = 2
  ,sample.variance.ADM.g2 = 1
  ,sample.size.g2 = 10
  ,null.hypothesis.difference = 0
  ,alternative = c("two.sided","less","greater") #difference in means only
  ,conf.level = 0.95
  ,g1.details = F #Include point estimates and confidence intervals for mean, variance, and sd
  ,g2.details = F #Include point estimates and confidence intervals for mean, variance, and sd
  #,details.include = c("mean", "mean.ci", "var") #,"var.ci" ,"sd", "sd.ci"
  ,t.test.impl = mean.t.test.twosample.independent.equal.variance.simple
) {
  
  validate.htest.alternative(alternative)
  
  retval <- t.test.impl(
    sample.mean.g1 = sample.mean.ADM.g1
    ,sample.variance.g1 = sample.variance.ADM.g1
    ,sample.size.g1 = sample.size.g1
    ,sample.mean.g2 = sample.mean.ADM.g2
    ,sample.variance.g2 = sample.variance.ADM.g2
    ,sample.size.g2 = sample.size.g2
    ,null.hypothesis.difference = null.hypothesis.difference
    ,alternative = alternative
    ,conf.level = conf.level
    ,g1.details = g1.details 
    ,g2.details = g2.details
  )
  
  
  retval$method <- "Two-Sample t Test For Average Median Absolute Deviation"
  names(retval$null.value) <- "mean ADM difference"
  names(retval$parameter) <- "null hypothesis mean ADM difference"
  
  
  retval
  
  
  
}

variability.test.twosample.independent.brown.forsythe.simple <- variability.test.twosample.independent.ADM.simple