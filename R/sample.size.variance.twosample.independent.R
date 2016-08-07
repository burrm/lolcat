sample.size.variance.twosample.independent <- function(
  variance.estimate.g1 = 2
  ,variance.estimate.g2 = 1
  ,alpha = .05
  ,beta = .1
  ,alternative = c("two.sided","greater","less")
  ,details = TRUE
  ,power.from.actual = F #report parameter power instead of true power
  
  
) {
  
  ratio <- variance.estimate.g1/variance.estimate.g2
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
    
    current.beta <- 1-power.variance.twosample.independent(
      variance.estimate.g1 = variance.estimate.g1
      ,variance.estimate.g2 = variance.estimate.g2
      ,sample.size.g1 = n
      ,sample.size.g2 = n
      ,alpha = alpha
      ,alternative = alternative
      ,details = FALSE
    )
    
    #print(current.beta)
    
    while (current.beta > beta) {
      n <- n+1
      
      current.beta <- 1-power.variance.twosample.independent(
        variance.estimate.g1 = variance.estimate.g1
        ,variance.estimate.g2 = variance.estimate.g2
        ,sample.size.g1 = n
        ,sample.size.g2 = n
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
    as.data.frame(list(test="F"
                       ,type = "two.sample"
                       ,alternative = alternative[1]
                       ,sample.size.g1 = n
                       ,sample.size.g2 = n
                       ,df.g1 = n - 1
                       ,df.g2 = n - 1
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

#sample.size.variance.twosample.independent()

# x<-seq(0,2,.01)
# y<-df(x,318,318)
# y2 <-(1/1.5)*df(x/1.5,318,318)
# 
# f <- function(x) {
#   (1/1.5)*df(x/1.5,318,318)
# }
# 
# plot(x,y,type="l",col="orange")
# lines(x,y2,col="blue")
# 
# pf(1.2463425, 318,318,ncp = 318*1.5/pi, lower.tail = F)
# 
# rmnames(integrate(f,1.2463425,Inf)$value)
# rmnames(integrate(f,0,1.2463425)$value)

