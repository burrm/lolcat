proportion.test.onesample.exact.simple <- function(
                                              sample.proportion 
                                             ,sample.size
                                             ,h0.proportion
                                             ,alternative = c("two.sided", "less", "greater")
                                             ,conf.level = .95
) {
  
  np <- sample.proportion*sample.size
  
  p.value <- if (alternative[1] == "two.sided") {
    if (sample.proportion < h0.proportion) {
      2*pbinom(np, sample.size, h0.proportion)
    } else {
      2*pbinom(np, sample.size, h0.proportion, lower.tail = F)
    }
  } else if (alternative[1] == "less") {
    pbinom(np, sample.size, h0.proportion, lower.tail = T)
  } else if (alternative[1] == "greater") {
    pbinom(np, sample.size, h0.proportion, lower.tail = F) + dbinom(np, sample.size, h0.proportion)
  } else {
    NA
  }
  
  #First guess?
  #cilower <- qbinom((1-conf.level)/2, size = sample.size, prob = sample.proportion)/sample.size
  #ciupper <- qbinom((1-conf.level)/2, size = sample.size, prob = sample.proportion, lower.tail = FALSE)/sample.size

  alpha2 <- (1-conf.level)/2
  
  # Confidence Intervals Adapted from binom package
  # x1 <- x == 0
  # x2 <- x == n
   lb <- ifelse(np == 0, 1, np)
   ub <- ifelse(np == sample.size,sample.size-1, np)
  # lb[x1] <- 1
  # ub[x2] <- n[x2] - 1
   lowerci <- 1 - qbeta(1 - alpha2, sample.size + 1 - np, lb)
   upperci <- 1 - qbeta(alpha2, sample.size - ub, np + 1)
  # if (any(x1))
  #   lcl[x1] <- rep(0, sum(x1))
   lowerci <- ifelse(np == 0, 0, lowerci)
   
  # if (any(x2)) 
  #   ucl[x2] <- rep(1, sum(x2))
   upperci <- ifelse(np == sample.size, 1, upperci)
   
   
     # res.exact <- data.frame(method = rep("exact", NROW(x)), 
  #                         xn, mean = p, lower = lcl, upper = ucl)
  # res <- if (is.null(res)) 
  #   res.exact
  # else rbind(res, res.exact)
  
  pow <- power.proportion.test.onesample.exact(
    sample.size = sample.size
    ,null.hypothesis.proportion = h0.proportion
    ,alternative.hypothesis.proportion = sample.proportion
    ,alternative = alternative
    ,alpha = 1-conf.level
    ,details = F
  )
  
  
  
    

  retval<-list(data.name   = "sample proportion and sample size",
               statistic   = c(p = sample.proportion), 
               estimate    = c(sample.prop = sample.proportion 
                               ,sample.size = sample.size
                               ,n.times.p = np
                               ,power = pow
                               
               ),
               parameter   = h0.proportion,
               p.value     = p.value,
               null.value  = h0.proportion,
               alternative = alternative[1],
               method      = "One-Sample Proportion Test (Exact)"
               ,conf.int    = c(lowerci,upperci)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "proportion"
  names(retval$parameter) <- "null hypothesis proportion"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}