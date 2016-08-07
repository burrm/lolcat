#Helped... http://math.stackexchange.com/questions/275648/multiplication-of-a-random-variable-with-constant
power.variance.twosample.independent <- function(
  sample.size.g1 = 1
  ,sample.size.g2 = 1
  ,variance.estimate.g1 = 2
  ,variance.estimate.g2 = 1
  ,h0.variance = 1
  ,alpha = .05
  ,alternative = c("two.sided","greater", "less")
  ,details = TRUE
  
  
) {
  ratio <- variance.estimate.g1 / variance.estimate.g2
  df.g1 <- sample.size.g1 - 1
  df.g2 <- sample.size.g2 - 1
    
  # Find Rejection Regions
  
  f.upper <- NA
  f.lower <- NA
  
  if (alternative[1] == "two.sided") {
    f.lower <- qf(alpha/2, df.g1, df.g2, lower.tail = T)
    f.upper <- qf(1-alpha/2, df.g1, df.g2, lower.tail = T)
  } else {
    f.lower <- qf(alpha, df.g1, df.g2, lower.tail = T)
    f.upper <- qf(1-alpha, df.g1, df.g2, lower.tail = T)
  }
  
  #print(paste(f.lower,f.upper))
  #  print(ratio)
  # Determine Alternative Hypothesis Distribution
  
  p.fn <- function(q, lower.tail = T) {
    f <- function(x) {
      (1/ratio)*df(x/ratio,df.g1,df.g2)
    }
    
    if (lower.tail) {
      rmnames(integrate(f,0,q)$value)
    } else {
      rmnames(integrate(f,q,Inf)$value)
    }
    
  }
  
  beta <- NA
  
  # Calculate beta
  if (ratio < 1) {
    if (alternative[1] == "two.sided") {
      beta <- p.fn(f.lower, lower.tail = F)
    } else if (alternative[1] == "less") {
      beta <- p.fn(f.lower, lower.tail = F)
    } else {
      beta <- p.fn(f.upper, lower.tail = T)
    }
    
  } else if (ratio > 1) {
    if (alternative[1] == "two.sided") {
      beta <- p.fn(f.upper, lower.tail = T)
    } else if (alternative[1] == "less") {
      beta <- p.fn(f.lower, lower.tail = F)
    } else {
      beta <- p.fn(f.upper, lower.tail = T)
    }
  } else {
    beta <- 1
  }
  
  pow <- 1 - beta  
  
  if (details) {
    as.data.frame(list(test="F"
                       ,type = "two.sample"
                       ,alternative = alternative[1]
                       ,sample.size.g1 = sample.size.g1
                       ,sample.size.g2 = sample.size.g2
                       ,df.g1 = df.g1
                       ,df.g2 = df.g2
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

# power.variance.twosample.independent(
#   variance.estimate.g1 = 1.5
#   ,variance.estimate.g2 = 1
#   ,sample.size.g1 = 251
#   ,sample.size.g2 = 251
# )