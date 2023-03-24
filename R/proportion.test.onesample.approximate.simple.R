#' One Sample Proportion Test (Approximate) 
#' 
#' Calculates a one-sample proportion test to determine if a sample 
#' proportion is statistically different from an assumed population proportion.
#'
#' @param sample.proportion Scalar/numeric - sample proportion between 0 and 1
#' @param sample.size Scalar/numeric - sample size.
#' @param null.hypothesis.proportion Scalar/numeric - assumed population proportion.
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#' @param x Vector - Sample Values
#' @param success.value Scalar - Value compared with x using == operator to determine if a trial is a "success" 
#' @param continuity.correction Scalar/logical - if TRUE, apply the continuity correction to the calculation
#'
#' @return Hypothesis test result showing results of test. 
proportion.test.onesample.approximate.simple <- function(
  sample.proportion 
  ,sample.size
  ,null.hypothesis.proportion = .5
  ,alternative = c("two.sided", "less", "greater")
  ,conf.level = .95
  ,continuity.correction = T
) {
  validate.htest.alternative(alternative = alternative)
  d <- sample.proportion - null.hypothesis.proportion
  
  if (continuity.correction) {
    if (alternative[1] == "two.sided") {
      d <- d + sign(null.hypothesis.proportion-sample.proportion)*1/(2*sample.size)
    } else if (alternative[1] == "greater") {
      d <- d - 1/(2*sample.size)
    } else if (alternative[1] == "less") {
      d <- d + 1/(2*sample.size)
    }
  }
  
  se.est <- sqrt(null.hypothesis.proportion*(1-null.hypothesis.proportion)/sample.size)
  ci.est <- sqrt(sample.proportion*(1-sample.proportion)/sample.size)
  
  
  z <- d/se.est
  
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  
  z.upper <- sample.proportion + cv*ci.est
  z.lower <- sample.proportion - cv*ci.est
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pnorm(z)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pnorm(z,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pnorm(z,lower.tail = TRUE)
  } else {
    NA
  }

  pow <- power.proportion.test.onesample.approximate(
    sample.size = sample.size
    ,null.hypothesis.proportion = null.hypothesis.proportion
    ,alternative.hypothesis.proportion = sample.proportion
    ,alternative = alternative
    ,alpha = 1-conf.level
    ,details = F
  )
    
  
  retval<-list(data.name   = "sample proportion and sample size",
               statistic   = c(z=z), 
               estimate    = c(sample.proportion = sample.proportion 
                               ,sample.size = sample.size
                               ,n.times.p = sample.size*sample.proportion
                               ,n.times.null.hypothesis.p = sample.size*null.hypothesis.proportion
                               ,se.est = se.est
                               ,power = pow
               ),
               parameter   = null.hypothesis.proportion,
               p.value     = p.value,
               null.value  = null.hypothesis.proportion,
               alternative = alternative[1],
               method      = "One-Sample Proportion Test (Approximate)",
               conf.int    = c(z.lower,z.upper)
  )
  
  #names(retval$estimate) <- c("sample proportion")
  names(retval$null.value) <- "proportion"
  names(retval$parameter) <- "null hypothesis proportion"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
  
  
  
  
}


#proportion.test.onesample.approximate.simple(sample.proportion = .8, sample.size = 25, null.hypothesis.proportion = .5, continuity.correction = F)

