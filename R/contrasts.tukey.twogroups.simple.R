# Kirk pg 187-189 and PHAST-TM help documentation
# Performs a single comparison between 2 group means using Tukey-Kramer test
contrasts.tukey.twogroups.simple <- function(
  weight = c(-1,1)                # vector with 2 contrast coefficients
  ,group.mean = c(1,-1)           # vector with 2 group means 
  ,group.sample.size = c(10,10)   # vector with 2 group sample sizes
  ,conf.level.familywise = .95    # 1 - Familywise type 1 error rate 
  ,n.means = 2                    # Number of means being compared
  ,mean.squared.error = 1         # AET mean square
  ,df.mean.squared.error = 1      # AET degrees of freedom
  ,alternative = c("greater")#, "two.sided", "less")
) {
  validate.htest.alternative(alternative = alternative)
  
  quantile.tukey <- sqrt(2)*sum(weight*group.mean)/sqrt(mean.squared.error*sum(1/group.sample.size))
  
  p.value <- 
    if (alternative[1] == "two.sided") {
      
      tmp<-ptukey(
        q = quantile.tukey
        ,nmeans= n.means
        ,df = df.mean.squared.error
      )
      min(tmp,1-tmp)*2
      
    } else if (alternative[1] == "greater") {
      
      ptukey(
        q = quantile.tukey
        ,nmeans= n.means
        ,df = df.mean.squared.error
        ,lower.tail = FALSE
      )
      
    } else if (alternative[1] == "less") {
      
      ptukey(
        q = quantile.tukey
        ,nmeans= n.means
        ,df = df.mean.squared.error
        ,lower.tail = TRUE
      )
      
    } else {
      NA
    }
  
  retval<-list(data.name   = "group means, sample sizes, mean square error, and degrees of freedom",
               statistic   = quantile.tukey/sqrt(2), 
               estimate    = c(df = df.mean.squared.error
                               ,n.means = n.means
                               ,mean.squared.error = mean.squared.error),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Tukey-Kramer Test for Contrast Differences",
               conf.int    = c(NA,NA)
  )
  
  names(retval$statistic) <- "t statistic"
  names(retval$null.value) <- "contrast difference"
  names(retval$parameter) <- "null hypothesis contrast difference"
  attr(retval$conf.int, "conf.level")  <- conf.level.familywise
  
  class(retval)<-"htest"
  retval
  
}