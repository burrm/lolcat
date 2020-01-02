# https://www.itl.nist.gov/div898/handbook/eda/section3/eda357.htm

variance.test.ksample.bartlett.simple <- function(
  sample.sizes = c(5,5,5)
  ,sample.variances = c(3,1,7)
  ,alternative = c("greater", "less", "two.sided")
) {
  validate.htest.alternative(alternative = alternative)
  
  local.df <- data.frame(n = sample.sizes, v = sample.variances)
  local.df <- local.df[which(local.df$v > 0 & local.df$n > 0),]

  k <- nrow(local.df)
  n <- sum(local.df$n)
  
  s_p=sum((local.df$n-1)*local.df$v/(n-k))
  
  num_1 <- (n-k)*log(s_p)
  num_2 <- sum((local.df$n-1)*log(local.df$v))
  
  denom<-1+ (1/(3*(k-1)))*((sum(1/(local.df$n-1))) -1/(n-k))
  
  chi.square.statistic <- (num_1-num_2)/denom
  
  df <- k-1
  
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pchisq(chi.square.statistic,df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pchisq(chi.square.statistic,df,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pchisq(chi.square.statistic,df,lower.tail = TRUE)
  } else {
    NA
  }
  
  retval<-list(data.name   = "sample variances and sample sizes",
               statistic   = chi.square.statistic, 
               estimate    = c(df = df
                               ,group.count = k
                               ,total.n = n
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Bartlett Test For Homogeneity of Variance"
               
  )
  
  names(retval$statistic) <- "chi-square statistic"
  names(retval$null.value) <- "variance"
  names(retval$parameter) <- "null hypothesis variance difference"

  class(retval)<-"htest"
  retval
  
  
}

#variance.test.ksample.bartlett.simple(sample.variances =c(4.1,4.3,4.2))

#variance.test.ksample.bartlett.box.simple



