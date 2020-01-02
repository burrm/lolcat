lin.mudholkar.normality.test.simple <- function(sample.r
                                         ,sample.size
                                         ,method = c("lin-mudholkar-1980")
                                         ,alternative = c("two.sided","less","greater")
                                         ,conf.level = 0.95
) {
  validate.htest.alternative(alternative = alternative)
  
  n <- sample.size

  r <- sample.r
  
  Y <- -11.7/n + 55.06/n^2
  S <- sqrt(3/n - 7.324/n^2 + 53.005/n^3)
  a <- 24/Y - 3
  c <- .5 * log((1+r)/(1-r))
  b <- (-24 * c) / (S*Y) 
  
  if (n < 4) {
    roots <-rep(NA,3)
  } else {
    roots <- polyroot(c(b,a,0,1))
  }
  roots[which(Im(roots) == 0)]
  roots <- Re(roots)
  
  roots <- roots[order(abs(roots))]
  z <- roots[1]
  
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
  
  
  
  retval<-list(data.name   = "input data",
               statistic   = z, 
               estimate    = c(sample.size = n
                               ,r = r
                               ,b = b
                               ,a = a
                               ,S = S
                               ,Y = Y
                               ,root.1 = roots[1]
                               ,root.2 = roots[2]
                               ,root.3 = roots[3]
                               
                               
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Lin-Mudholkar Normality Test"
  )
  
  names(retval$statistic) <- "z statistic"
  names(retval$null.value) <- "z"
  names(retval$parameter) <- "null hypothesis z"
  
  class(retval)<-"htest"
  retval
  
}
