#' Shape Tests - Poisson Distribution Test 
#' 
#' Calculates a test to see if the sample data cannot be assumed to be Poisson distributed..
#'
#' @param x Vector/numeric - Sample Values
#'
#' @return Hypothesis test result showing results of test. Low p value rejects assumption of data fitting Poisson distribution.
poisson.dist.test <-
function(x
         #,conf.level = .95
         ) {
  #Based on MVPStats - http://mvpprograms.com/help/mvpstats/distributions/PoissonDistributionTest
  x <- na.omit(x)
  n<-length(x)
  chisq.val<-(n-1)*var(x)/mean(x)
  
  #Not matching MVPStats
  p.value<-pchisq(chisq.val,n-1,lower.tail=F)
  p.value<-2*min(p.value,1-p.value)

  retval<-list(data.name   = "input data",
               statistic   = chisq.val, 
               estimate    = c(chisq.val, var(x), mean(x)),
               parameter   = n-1 ,
               p.value     = p.value,
               null.value  = n-1,
               alternative = "two.sided",
               method      = "Poisson Distribution Fit Test Using Variance and Mean"#,
#               conf.int    = c(NA,NA)
               )

  names(retval$estimate) <- c("chi.square","sample variance", "sample mean")
  names(retval$statistic) <- "chi.square"
  names(retval$null.value) <- "chi.square"
  names(retval$parameter) <- "degrees of freedom"
#  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}