cor.yule.q <- function(
  x
  ,alternative = c("two.sided","less","greater")
) {
  validate.htest.alternative(alternative = alternative)
  
  a <- x[1,1]
  b <- x[1,2]
  c <- x[2,1]
  d <- x[2,2]
  
  Q <- (a*d - b*c)/(a*d + b*c)
  
  z <- Q/sqrt(.25*((1-Q^2)^2)*(1/a + 1/b + 1/c + 1/d) )
  
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
               statistic   = c(z = z), 
               estimate    = c( 
                 Q = Q
               ),
               parameter   = 0 ,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Yule's Q"#,
               #                 conf.int    = c(NA,NA)
  )
  
  names(retval$null.value) <- "Q"
  names(retval$parameter) <- "null hypothesis Q"
  
  class(retval)<-"htest"
  retval
}