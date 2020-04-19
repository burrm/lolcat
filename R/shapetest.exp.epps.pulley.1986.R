shapetest.exp.epps.pulley.1986 <- function(
    x
) {
  alternative <- "two.sided"
  validate.htest.alternative(alternative = alternative)
  
  if (any(x <= 0)) {
      warning("Values of x should be greater than 0.")
  }
  
  sample.size <- length(x)
  sample.mean <- mean(x)

  x.ep <- exp(-1*x/sample.mean)
  z       <- sqrt(48*sample.size)*(sum(x.ep)/sample.size - .5)
  
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
    
  retval<-list(data.name   = "x",
               statistic   = z, 
               estimate    = c(sample.size = sample.size
                               
                               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Epps and Pulley Exponentiality Test (1986)"
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$statistic) <- "z statistic"
  names(retval$null.value) <- "test parameter"
  names(retval$parameter) <- "null hypothesis test parameter"
  # attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}