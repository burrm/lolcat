#Formula from MVP
variance.test.ksample.bartlett.box.simple <-function(
  sample.sizes = c(5,5,5)
  ,sample.variances = c(3,1,7)
  ,alternative = c("greater", "less", "two.sided")
) {
  validate.htest.alternative(alternative = alternative)
  
  local.df <- data.frame(n = sample.sizes, v = sample.variances)
  local.df <- local.df[which(local.df$v > 0 & local.df$n > 0),]
  
  k <- nrow(local.df)
  n <- sum(local.df$n)
  
  df.within <- n-k
  
  M <- df.within*log(sum((local.df$n-1)*local.df$v)/df.within)-sum((local.df$n-1)*log(local.df$v))
  A <- (1/(3*(k-1)))*(sum(1/(local.df$n-1)-1/df.within))
  f2 <- (k+1)/(A*A)
  f1 <- 1-A+2/f2
  
  f.statistic <-  (f2*M)/((k-1)*(f2/f1-M))
    
  df.g1<-k-1
  df.g2<-f2
    
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pf(f.statistic,df.g1,df.g2)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pf(f.statistic,df.g1,df.g2,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pf(f.statistic,df.g1,df.g2,lower.tail = TRUE)
  } else {
    NA
  }
  
  
  retval<-list(data.name   = "sample variances and sample sizes",
               statistic   = f.statistic, 
               estimate    = c(df.g1 = df.g1
                               ,df.g2 = df.g2
                               ,group.count = k
                               ,total.n = n
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Bartlett-Box Test For Homogeneity of Variance"
               
  )
  
  names(retval$statistic) <- "F statistic"
  names(retval$null.value) <- "variance"
  names(retval$parameter) <- "null hypothesis variance difference"
  
  class(retval)<-"htest"
  retval
}


#variance.test.ksample.bartlett.box.simple(
#  sample.sizes = c(5,5,5)
#  ,sample.variances = c(3,100,10)
#  ,alternative = "greater"
#)
