
cor.spearman.rank.simple <- function (
  r_sp
  ,sample.size
  ,conf.level = .95
  ,alternative = c("two.sided","less","greater")
) {
  validate.htest.alternative(alternative = alternative)
  
  n <- sample.size
  
  # r_sp <- cor(x1,x2,method="spearman")
  # n <- length(x1)
  #se.est <- 1/sqrt(length(x1)-1)
  
  t <- NA
  
  if (abs(r_sp)<1) {
    t <- r_sp*sqrt(n-2)/sqrt(1-r_sp^2)
  }
  
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pt(t, n-2)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pt(t,n-2,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pnorm(t,n-2,lower.tail = TRUE)
  } else {
    NA
  }
    
  z_r <- .5*log((1+r_sp)/(1-r_sp)) 
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  
  z_r.upper <- z_r + cv*sqrt((1+.5*(r_sp^2))/(n-3))
  z_r.lower <- z_r - cv*sqrt((1+.5*(r_sp^2))/(n-3))
  
  upperci <- (exp(2*z_r.upper) -1) / (exp(2*z_r.upper) +1)
  lowerci <- (exp(2*z_r.lower) -1) / (exp(2*z_r.lower) +1) 
  
  
  retval<-list(data.name   = "data",
               statistic   = c(t = t), 
               estimate    = c(
                 r_sp = r_sp
                 ,df = n-2
               ),
               parameter   = 0 ,
               p.value     = p.value,
               null.value  = 0,
               alternative = "two.sided",
               method      = "Spearman Rank Correlation Coefficient",
               conf.int    = c(lowerci,upperci)
  )
  
  names(retval$null.value) <- "rho_sp"
  names(retval$parameter) <- "null hypothesis rho_sp"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
  
}