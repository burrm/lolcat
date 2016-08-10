#TODO: Eventually find an exact calculation

cor.tetrachoric <- function(
  x #a 2x2 contingency table - array, matrix, data.frame
  ,alternative = c("two.sided","less","greater")
  ,conf.level = .95
  ,method = c("cosine", "sine")
) {
  
  a <- x[1,1]
  b <- x[1,2]
  c <- x[2,1]
  d <- x[2,2]
  
  n <- sum(x)
  
  x <- as.matrix(x)
  x <- xt.sums(x)
  
  p0y <- x[1,3]/x[3,3]
  p1y <- x[2,3]/x[3,3]
  
  p0x <- x[3,1]/x[3,3]
  p1x <- x[3,2]/x[3,3]
  
  hx <- dnorm(qnorm(max(p0x, p1x)))
  hy <- dnorm(qnorm(max(p0y, p1y)))
    
  r_tet <- if (method[1] == "sine") {
    sin((2*pi/360) * 90 * ((a+d-b-c)/n))
  } else if (method[1] == "cosine") {
    cos((2*pi/360)*180/(1+sqrt((a*d)/(b*c))))
  }
  
  se.est <- sqrt(p0x*p1x*p0y*p1y)/(hx*hy*sqrt(n))
  
  z <- r_tet / se.est
  
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
  
  retval<-list(data.name   = "dichotomized variable contingency table",
               statistic   = c(z =z), 
               estimate    = c(r_tet = r_tet
                               ,se.est = se.est
                               ,h.row = hy
                               ,h.col = hx
                               ,sample.size = n
                               ,prop.row.0 = p0y
                               ,prop.row.1 = p1y
                               ,prop.col.0 = p0x
                               ,prop.col.1 = p1x
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = paste("Tetrachoric Correlation Coefficient", 
                                   ifelse(method[1] == "cosine", "(Cosine Approximation)", "(Sine Approximation)")
                                   ),
               conf.int    = c(NA,NA)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "tetrachoric correlation"
  names(retval$parameter) <- "null hypothesis tetrachoric correlation"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}