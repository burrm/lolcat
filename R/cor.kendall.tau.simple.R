cor.kendall.tau.simple <- function(
   count.concordant = 0
  ,count.discordant = 0
  ,ties.x1 = NA #vector of score count tied for particular score
  ,ties.x2 = NA #vector of score count tied for particular score
  ,sample.size = .5*(1+sqrt(8*(count.concordant+count.discordant) + 1)) #Number of subjects in x1 or x2
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  tau     <- NA
  z       <- NA
  p.value <- NA
  
  n <- sample.size
  
  t1 <- NA
  t2 <- NA
  
  if (is.na(ties.x1) && is.na(ties.x2)) {
    tau <- (count.concordant - count.discordant)/(.5*n*(n-1))
  } else {
    S <- count.concordant - count.discordant
    
    d1 <-n*(n-1)
    
    if (!is.na(ties.x1)) {
      t1 <- sum(sapply(ties.x1, FUN = function(x) {
        x^2 - x
      }))
    }
    
    if (!is.na(ties.x2)) {
      t2 <- sum(sapply(ties.x2, FUN = function(x) {
        x^2 - x
      }))
    }
    
    tau <- 2*S/(sqrt(d1 - t1) * sqrt(d1 - t2))
    
  }
  
  z <- 3*tau*sqrt(n*(n-1))/sqrt(2*(2*n+5))
  
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
  
  
  retval<-list(data.name   = "concordant pairs, discordant pairs, and ties",
               statistic   = z, 
               estimate    = c(tau = tau 
                               ,sample.size = n
                               ,count.concordant = count.concordant
                               ,count.discordant = count.discordant
                               #,Tx = t1
                               #,Ty = t2
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Kendall's Tau",
               conf.int    = c(NA, NA)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$statistic) <- "z"
  names(retval$null.value) <- "tau"
  names(retval$parameter) <- "null hypothesis tau"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}