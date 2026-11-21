sample.size.proportion.test.onesample.exact <- function(null.hypothesis.proportion
                                                        ,alternative.hypothesis.proportion
                                                        ,alpha = .05
                                                        ,beta = .1
                                                        ,alternative = c("two.sided","less","greater")
                                                        ,details = TRUE
                                                        ,power.from.actual = F #report parameter power instead of true power
) {
  validate.htest.alternative(alternative = alternative)
  
  if (alternative[1] == "less" & alternative.hypothesis.proportion > null.hypothesis.proportion) {
    n <- NA
  } else if (alternative[1] == "greater" & alternative.hypothesis.proportion < null.hypothesis.proportion) {
    n <- NA
  } else {
    
    n <- 0
    last.beta <- 1
    
    while (last.beta > beta) {
      n <- n+1
      
      last.beta <- 1-power.proportion.test.onesample.exact(sample.size = n, 
                                                      null.hypothesis.proportion = null.hypothesis.proportion,
                                                      alternative.hypothesis.proportion = alternative.hypothesis.proportion,
                                                      alpha=alpha,
                                                      alternative = alternative,
                                                      details=FALSE
      )
      
    }
  }

  n.rounded <- ceiling(n)
  
  if (power.from.actual) {
    
    power <- 1-beta
    
  } else {
    
    #beta <- pnorm(sqrt(n.rounded) * effect.size/se.est  - qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
    #           , lower.tail = F)
    
    power <- power.proportion.test.onesample.exact(sample.size = n.rounded, 
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


