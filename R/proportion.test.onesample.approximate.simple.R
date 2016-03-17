proportion.test.onesample.approximate.simple <- function(
  sample.proportion 
  ,sample.size
  ,h0.proportion
  ,alternative = c("two.sided", "less", "greater")
  ,conf.level = .95
  ,continuity.correction = T
) {
  d <- sample.proportion - h0.proportion
  
  if (continuity.correction) {
    if (alternative[1] == "two.sided") {
      d <- d + sign(h0.proportion-sample.proportion)*1/(2*sample.size)
    } else if (alternative[1] == "greater") {
      d <- d - 1/(2*sample.size)
    } else if (alternative[1] == "less") {
      d <- d + 1/(2*sample.size)
    }
  }
  
  se.est <- sqrt(h0.proportion*(1-h0.proportion)/sample.size)
  
  z <- d/se.est
  
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  
  z.upper <- sample.proportion + cv*se.est
  z.lower <- sample.proportion - cv*se.est
  
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
  
  
  retval<-list(data.name   = "sample proportion and sample size",
               statistic   = c(z=z), 
               estimate    = c(sample.proportion = sample.proportion 
                               ,sample.size = sample.size
                               ,n.times.p = sample.size*sample.proportion
                               ,n.times.h0.p = sample.size*h0.proportion
                               ,se.est = se.est
                               
               ),
               parameter   = h0.proportion,
               p.value     = p.value,
               null.value  = h0.proportion,
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


#proportion.test.onesample.approximate.simple(sample.proportion = .8, sample.size = 25, h0.proportion = .5, continuity.correction = F)

