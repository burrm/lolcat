# Kirk pg 190 and PHAST-TM help documentation
# Performs a single comparison between 2 group means using Games-Howell test
contrasts.games.howell.twogroups.simple <- function(
  weight = c(-1,1)                # vector with 2 contrast coefficients
  ,group.mean = c(1,-1)           # vector with 2 group means 
  ,group.variance = c(1,1)        # vector with 2 group variances
  ,group.sample.size = c(10,10)   # vector with 2 group sample sizes
  ,conf.level.familywise = .95    # 1 - Familywise type 1 error rate 
  ,n.means = 2                    # Number of means being compared
  ,mean.squared.error = NA # If not NA, use Kirk equation, if NA, use PHAST-TM equation
  ,alternative = c("greater")#, "two.sided", "less")
) {
  validate.htest.alternative(alternative = alternative)
  
  quantile.games.howell <- NA
  
  if (!is.na(mean.squared.error)) {
    quantile.games.howell <- sqrt(2)*sum(weight*group.mean)/sqrt(mean.squared.error * sum(1/group.sample.size))
  } else {
    quantile.games.howell <- sqrt(2)*sum(weight*group.mean)/sqrt(sum(group.variance/group.sample.size))
  }
  
  df <- contrasts.df.welch.simple(
    weight = weight                          # vector with 2 or more contrast coefficients
    ,group.variance = group.variance         # vector with 2 or more group variances
    ,group.sample.size = group.sample.size   # vector with 2 or more group sample sizes
  )
  
  p.value <- 
    if (alternative[1] == "two.sided") {
    
      tmp<-ptukey(
                  q = quantile.games.howell
                  ,nmeans= n.means
                  ,df = df
                  )
      min(tmp,1-tmp)*2
  
    } else if (alternative[1] == "greater") {
    
      ptukey(
              q = quantile.games.howell
             ,nmeans= n.means
             ,df = df
             ,lower.tail = FALSE
      )
    
    } else if (alternative[1] == "less") {
    
      ptukey(
              q = quantile.games.howell
             ,nmeans= n.means
             ,df = df
             ,lower.tail = TRUE
      )
    
    } else {
      NA
    }
  
  retval<-list(data.name   = "group means, variances, and sample sizes",
               statistic   = quantile.games.howell/sqrt(2), 
               estimate    = c(df = df, n.means = n.means, mean.squared.error = mean.squared.error),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Games-Howell Test for Contrast Differences",
               conf.int    = c(NA,NA)
  )
  
  names(retval$statistic) <- "t statistic"
  names(retval$null.value) <- "contrast difference"
  names(retval$parameter) <- "null hypothesis contrast difference"
  attr(retval$conf.int, "conf.level")  <- conf.level.familywise
  
  class(retval)<-"htest"
  retval
  
}