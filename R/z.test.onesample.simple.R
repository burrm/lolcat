z.test.onesample.simple<-function(sample.mean
                                  ,known.population.variance = 1
                                  ,sample.size
                                  ,h0.mean = 0
                                  ,alternative = c("two.sided","less","greater")
                                  ,conf.level = 0.95
                                  ,finite.population.N = NA
) {
  se.est  <- sqrt(known.population.variance/sample.size)
  
  if (!is.na(finite.population.N)) {
    se.est <- se.est * sqrt((finite.population.N-sample.size)/(finite.population.N-1))
  }
  
  z       <- (sample.mean-h0.mean)/se.est
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  
  z.upper <- sample.mean + cv*se.est
  z.lower <- sample.mean - cv*se.est
  
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
  
  pow <- power.mean.z.onesample(sample.size = sample.size
                               ,effect.size = if (alternative[1] == "two.sided") {
                                  abs(sample.mean-h0.mean)
                               } else if (alternative[1] == "greater") {
                                  sample.mean - h0.mean
                               } else {
                                  h0.mean - sample.mean
                               }

                               ,variance = known.population.variance
                               ,alpha = 1-conf.level
                               ,alternative = alternative
                               ,details = F)
  
  retval<-list(data.name   = deparse(substitute(sample.mean)),
               statistic   = z, 
               estimate    = c(sample.mean = sample.mean 
                               ,sample.size = sample.size
                               ,se.est = se.est
                               ,power = pow
                               ),
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


#z.test.onesample.simple(sample.mean = .5, known.population.variance = 1, sample.size = 4,h0.mean = 0, alternative = "less")
