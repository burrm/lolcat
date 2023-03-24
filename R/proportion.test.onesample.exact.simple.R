#' One Sample Proportion Test (Exact) 
#' 
#' Calculates a one-sample proportion test to determine if a sample 
#' proportion is statistically different from an assumed population proportion.
#' 
#'
#' @param sample.proportion Scalar/numeric - sample proportion between 0 and 1
#' @param sample.size Scalar/numeric - sample size.
#' @param null.hypothesis.proportion Scalar/numeric - assumed population proportion.
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#' @param x Vector - Sample Values
#' @param success.value Scalar - Value compared with x using == operator to determine if a trial is a "success" 
#'
#' @return Hypothesis test result showing results of test. 
proportion.test.onesample.exact.simple <- function(
                                              sample.proportion 
                                             ,sample.size
                                             ,null.hypothesis.proportion = .5
                                             ,alternative = c("two.sided", "less", "greater")
                                             ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  np <- sample.proportion*sample.size
  
  p.value <- if (alternative[1] == "two.sided") {
    if (sample.proportion < null.hypothesis.proportion) {
      2*pbinom(np, sample.size, null.hypothesis.proportion) 
    } else {
      2*(pbinom(np, sample.size, null.hypothesis.proportion, lower.tail = F) + dbinom(np, sample.size, null.hypothesis.proportion))
    }
  } else if (alternative[1] == "less") {
    pbinom(np, sample.size, null.hypothesis.proportion, lower.tail = T)
  } else if (alternative[1] == "greater") {
    pbinom(np, sample.size, null.hypothesis.proportion, lower.tail = F) + dbinom(np, sample.size, null.hypothesis.proportion)
  } else {
    NA
  }
  
  if (p.value > 1) {
    p.value <- 1
  } else if (p.value < 0) (
    p.value <- 0
  )

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
    ,null.hypothesis.proportion = null.hypothesis.proportion
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
               parameter   = null.hypothesis.proportion,
               p.value     = p.value,
               null.value  = null.hypothesis.proportion,
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