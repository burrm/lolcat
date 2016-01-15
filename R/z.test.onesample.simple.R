z.test.onesample.simple<-function(sample.mean
                                  ,known.population.variance
                                  ,n
                                  ,h0.mean
                                  ,alternative = c("two-sided","less","greater")
                                  ,conf.level = 0.95
                                  ,finite.population.N = NA
) {
  se.est  <- sqrt(known.population.variance/n)
  
  if (!is.na(finite.population.N)) {
    se.est <- se.est * sqrt((finite.population.N-n)/(finite.population.N-1))
  }
  
  z       <- (sample.mean-h0.mean)/se.est
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  
  z.upper <- sample.mean + cv*se.est
  z.lower <- sample.mean - cv*se.est
  
  p.value <- if (alternative[1] == "two-sided") {
    tmp<-pnorm(z)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "less") {
    pnorm(z,lower.tail = FALSE)
  } else if (alternative[1] == "greater") {
    pnorm(z,lower.tail = TRUE)
  } else {
    NA
  }
  
  retval<-list(data.name   = deparse(substitute(sample.mean)),
               statistic   = z, 
               estimate    = c(sample.mean = sample.mean, se.est = se.est),
               parameter   = h0.mean,
               p.value     = p.value,
               null.value  = h0.mean,
               alternative = alternative[1],
               method      = "One-Sample Z Test For Means",
               conf.int    = c(z.lower,z.upper)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$statistic) <- "z statistic"
  names(retval$null.value) <- "mean"
  names(retval$parameter) <- "null hypothesis mean"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}