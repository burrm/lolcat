cor.j.index.onesample <- function(x
                                  ,conf.level = .95
                                  ,alternative = c("two.sided","less","greater")
                                  ) {
  validate.htest.alternative(alternative = alternative)
  a <- x[1,1]
  b <- x[1,2]
  c <- x[2,1]
  d <- x[2,2]
  
  j <- (a*d - b*c) /((a+b)*(c+d))
  se.est <- sqrt( a*b/(a+b)^3 + c*d/(c+d)^3 )
  
  z = j/se.est
  
  if (a+b+c+d < 40) {
    warn("j-index total n < 40... consider results carefully")
  }
  
  cv  <- qnorm(conf.level+(1-conf.level)/2)
  
  upperci <- j + cv*se.est
  lowerci <- j - cv*se.est
  
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
  
  
  
  retval<-list(data.name   = "data",
               statistic   = c(J = j), 
               estimate    = c(se.est = se.est
               ),
               parameter   = 0 ,
               p.value     = p.value,
               null.value  = 0,
               alternative = "two.sided",
               method      = "J Index of Predictive Efficiency",
               conf.int    = c(lowerci,upperci)
  )
  
  names(retval$null.value) <- "J"
  names(retval$parameter) <- "null hypothesis J"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}