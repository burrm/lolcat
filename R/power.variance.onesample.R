power.variance.onesample <- function(
  sample.size = 1
  ,null.hypothesis.variance = 1
  ,alternative.hypothesis.variance = 2
  ,alpha = .05
  ,alternative = c("two.sided","greater", "less")
  ,details = TRUE
  
  
) {
  validate.htest.alternative(alternative = alternative)
  ratio <- alternative.hypothesis.variance/null.hypothesis.variance
  #ratio <- null.hypothesis.variance/alternative.hypothesis.variance # Don't do this
  n <- sample.size

  # Find Rejection Regions
  
  chi.upper <- NA
  chi.lower <- NA
  
  if (alternative[1] == "two.sided") {
    chi.lower <- qchisq(alpha/2, n-1, lower.tail = T)
    chi.upper <- qchisq(1-alpha/2, n-1, lower.tail = T)
  } else {
    chi.lower <- qchisq(alpha, n-1, lower.tail = T)
    chi.upper <- qchisq(1-alpha, n-1, lower.tail = T)
  }
  
  #print(paste(chi.lower,chi.upper))
  
  # Determine Alternative Hypothesis Distribution
  #n.scaled <- (n-1)*ratio
  
  p.fn <- function(q, lower.tail = T) {
    pgamma(q = q, shape = (n-1)/2, scale= 2*ratio, lower.tail = lower.tail)# pchisq(df = n.scaled, lower.tail = lower.tail)
  }
  
  beta <- NA
  
  # Calculate beta
  if (ratio < 1) {
    if (alternative[1] == "two.sided") {
      beta <- p.fn(chi.lower, lower.tail = F)
    } else if (alternative[1] == "less") {
      beta <- p.fn(chi.lower, lower.tail = F)
    } else {
      beta <- p.fn(chi.upper, lower.tail = T)
    }
    
  } else if (ratio > 1) {
    if (alternative[1] == "two.sided") {
      beta <- p.fn(chi.upper, lower.tail = T)
    } else if (alternative[1] == "less") {
      beta <- p.fn(chi.lower, lower.tail = F)
    } else {
      beta <- p.fn(chi.upper, lower.tail = T)
    }
  } else {
    beta <- 1
  }
  
  pow <- 1 - beta  
  
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
    pow
  }
  
  
}

# power.variance.onesample(
#   sample.size = 21
#   ,alternative.hypothesis.variance = 2
#   ,null.hypothesis.variance = 1
#   ,alpha = .05
#   ,alternative = "two.sided"
# )

# pgamma(34.1696069, 10, scale = 4, lower.tail = F)
# 
# 
# x <- seq(0,50,.01)
# y <- dgamma(x, shape = 10, scale = 4)
# 
# plot(x,y,type="l",col="blue")
# 
# y2 <- dchisq(x, df = 20)
# 
# lines(x,y2,col="orange")
# 
# curve(function(x) {dgamma(x, 10, 2)}, 0,50)
# 
# ?dgamma
# 
# 
# ?pchisq
# 
# d<-data.frame(n = 20:50)
# 
# d$pow <-sapply(d$n, FUN=function(n) {
#   pchisq(34.1696069, n, lower.tail = F)  
# })
# 
# d
