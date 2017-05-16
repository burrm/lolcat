power.proportion.test.onesample.exact <- function(sample.size
                                                  ,null.hypothesis.proportion
                                                  ,alternative.hypothesis.proportion
                                                  ,alpha = .05
                                                  ,alternative = c("two.sided","less","greater")
                                                  ,details = T
) {
  validate.htest.alternative(alternative = alternative)
  
  n <- sample.size
  
  np.null <- sample.size * null.hypothesis.proportion
  np.alt  <- sample.size * alternative.hypothesis.proportion
  
  if (null.hypothesis.proportion < alternative.hypothesis.proportion) {
    #right tail of null dist for alpha
    cv <- qbinom(ifelse(alternative[1] == "two.sided", alpha/2, alpha)
                 ,n
                 ,null.hypothesis.proportion
                 ,lower.tail = F)
    
    pow <- pbinom(cv, n, alternative.hypothesis.proportion, lower.tail = F) 
    
  } else {
    #left tail of null dist for alpha
    cv <- qbinom(ifelse(alternative[1] == "two.sided", alpha/2, alpha)
                 ,n
                 ,null.hypothesis.proportion
                 ,lower.tail = T)
    
    pow <- pbinom(cv, n, alternative.hypothesis.proportion, lower.tail = T) - dbinom(cv, n, alternative.hypothesis.proportion)
    
  }
  
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