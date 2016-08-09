cor.pearson.r.onesample.simple <- function(
                          sample.r,
                          sample.size,
                          null.hypothesis.rho = 0,
                          alternative = c("two.sided","less","greater"),
                          conf.level = .95
) {
  
  r <- sample.r
  n <- sample.size

  z_r <- .5*log((1+r)/(1-r))
  
  if (null.hypothesis.rho == 0) {
    t <- r*sqrt(n-2)/sqrt(1-r^2) 
    df <- n-2
    estimate = c(sample.r = sample.r, 
                 df = df)
    
    statistic <- c(t.statistic = t)
    
    p.value <- if (alternative[1] == "two.sided") {
      tmp<-pt(t, df)
      min(tmp,1-tmp)*2
    } else if (alternative[1] == "greater") {
      pt(t, df, lower.tail = FALSE)
    } else if (alternative[1] == "less") {
      pt(t, df, lower.tail = TRUE)
    } else {
      NA
    }
    
  } else {
    z_null.hypothesis<- .5*log((1+null.hypothesis.rho)/(1-null.hypothesis.rho))
    z <- (z_r-z_null.hypothesis)/sqrt(1/(n-3))
    
    estimate = c(sample.r = sample.r, 
                 z_null.hypothesis= z_null.hypothesis)
    
    statistic <- c(z.statistic = z)
    
    p.value <- if (alternative[1] == "two.sided") {
      tmp<-pnorm(z)
      min(tmp,1-tmp)*2
    } else if (alternative[1] == "greater") {
      pnorm(z, lower.tail = FALSE)
    } else if (alternative[1] == "less") {
      pnorm(z, lower.tail = TRUE)
    } else {
      NA
    }
    
  }
  

  cv      <- qnorm(conf.level+(1-conf.level)/2)

  z_r.lowerci <- z_r - cv*sqrt(1/(n-3))
  z_r.upperci <- z_r + cv*sqrt(1/(n-3))
  
  #Need to optimize for inverse of Fisher's z_r transform
  f <- function(x, z0) {(z0 - .5 * log((1+x)/(1-x)))^2  }
  f.lower <- function(x) { f(x, z_r.lowerci) }
  f.upper <- function(x) { f(x, z_r.upperci) }
  
  lowerci <- optimize(f.lower, interval=c(-1,1))$minimum
  upperci <- optimize(f.upper, interval=c(-1,1))$minimum
  
  
  
  retval<-list(data.name   = "sample r and sample size",
               statistic   = statistic, 
               estimate    = c(estimate,
                               sample.size = n,
                               r.squared = r^2,
                               z_r.lowerci = z_r.lowerci,
                               z_r = z_r,
                               z_r.upperci = z_r.upperci
                               ),
               parameter   = null.hypothesis.rho,
               p.value     = p.value,
               null.value  = null.hypothesis.rho,
               alternative = alternative[1],
               method      = "One-Sample Test for Pearson Product Moment Correlation",
               conf.int    = c(lowerci,upperci)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "correlation"
  names(retval$parameter) <- "null hypothesis correlation"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}