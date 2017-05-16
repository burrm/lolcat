sample.size.variance.onesample <- function(
  null.hypothesis.variance = 1
  ,alternative.hypothesis.variance = 2
  ,alpha = .05
  ,beta = .1
  ,alternative = c("two.sided","greater","less")
  ,details = TRUE
  ,power.from.actual = F #report parameter power instead of true power
  
  
) {
  validate.htest.alternative(alternative = alternative)
  
  ratio <- alternative.hypothesis.variance/null.hypothesis.variance
  n <- 2  

  if (ratio < .9991) {
    if (alternative[1] == "two.sided") {
      
    } else if (alternative[1] == "greater") {
      n <- NA  
    } else if (alternative[1] == "less") {
      
    } else {
      n <- NA
    }
    
  } else if (ratio > 1.0001) {
    
    if (alternative[1] == "two.sided") {
      
    } else if (alternative[1] == "greater") {
        
    } else if (alternative[1] == "less") {
      n <- NA
    } else {
      n <- NA
    }
    
  } else {
    n <- NA
    warning("Ratio close to 1 will not converge.")
  }
  
  current.beta <- NA
  
  if (!is.na(n)) {
  
    current.beta <- 1-power.variance.onesample(
      sample.size = n
      ,alternative.hypothesis.variance = alternative.hypothesis.variance
      ,null.hypothesis.variance = null.hypothesis.variance
      ,alpha = alpha
      ,alternative = alternative
      ,details = FALSE
    )

    #print(current.beta)
          
    while (current.beta > beta) {
      n <- n+1
    
      current.beta <- 1-power.variance.onesample(
        sample.size = n
        ,null.hypothesis.variance = null.hypothesis.variance
        ,alternative.hypothesis.variance = alternative.hypothesis.variance
        ,alpha = alpha
        ,alternative = alternative
        ,details = FALSE
      )
    }
  
  }

  if (power.from.actual) {
    
  } else {
    
    beta <- current.beta
  }
  
  if (details) {
    as.data.frame(list(test="chi-square"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = n
                       ,df = n - 1
                       ,ratio = ratio
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = beta
                       ,power = 1- beta
    ))
    
  }
  else {
    n
  }
    
}

#sample.size.variance.onesample(alternative.hypothesis.variance = 2, null.hypothesis.variance = 1)
#qchisq(.025, 10, lower.tail = F)

