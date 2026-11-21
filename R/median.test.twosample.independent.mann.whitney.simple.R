median.test.twosample.independent.mann.whitney.simple <- function(
  sum.ranks.g1
  ,sum.ranks.g2
  ,sample.size.g1
  ,sample.size.g2
  ,alternative = c("two.sided","less","greater")
) {
  validate.htest.alternative(alternative = alternative)
  
  U1 <- sample.size.g1*sample.size.g2 + sample.size.g1*(sample.size.g1+1)/2 - sum.ranks.g1
  U2 <- sample.size.g1*sample.size.g2 + sample.size.g2*(sample.size.g2+1)/2 - sum.ranks.g2
  
  U <- min(U1, U2)
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pwilcox(U, sample.size.g1,sample.size.g2)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    tmp <- pwilcox(U, sample.size.g1,sample.size.g2)
    
    if (sum.ranks.g1 < sum.ranks.g2) {
      tmp <- 1-tmp
    }
    
    tmp
  } else if (alternative[1] == "less") {
    tmp <- pwilcox(U, sample.size.g1,sample.size.g2)
    if (sum.ranks.g1 > sum.ranks.g2) {
      tmp <- 1-tmp
    }
    
    tmp
  } else {
    NA
  }
  
  
  
  retval<-list(data.name   = "sample ranks",
               statistic   = c(Mann.Whitney.U = U), 
               estimate    = c(sum.ranks.g1 = sum.ranks.g1 
                               ,sum.ranks.g2 = sum.ranks.g2
                               ,sample.size.g1 = sample.size.g1
                               ,sample.size.g2 = sample.size.g2
                               
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Mann-Whitney U Test"
               #,conf.int    = c(z.lower,z.upper)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "median difference"
  names(retval$parameter) <- "null hypothesis median difference"
  #attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
  
}


mann.whitney.u.test.simple <- median.test.twosample.independent.mann.whitney.simple
