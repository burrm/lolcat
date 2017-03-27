median.test.onesample.wilcoxon.simple <- function(
  sum.ranks.positive = 0
  ,sum.ranks.negative = 0
  ,adj.sample.size = NA
  ,null.hypothesis.location = 0
  ,alternative = c("two.sided","less","greater")
) {
  validate.htest.alternative(alternative = alternative)
  wilcoxon.t <- min(abs(c(sum.ranks.positive, sum.ranks.negative)))
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-psignrank(wilcoxon.t, adj.sample.size)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    tmp <- psignrank(wilcoxon.t, adj.sample.size)
    if (sum.ranks.negative > sum.ranks.positive) {
      tmp <- 1-tmp
    }
    
    tmp
  } else if (alternative[1] == "less") {
    tmp <- psignrank(wilcoxon.t, adj.sample.size)
    if (sum.ranks.positive > sum.ranks.negative) {
      tmp <- 1-tmp
    }
    
    tmp
  } else {
    NA
  }
  
  
  
  retval<-list(data.name   = "difference ranks",
               statistic   = c(Wilcoxon.T = wilcoxon.t), 
               estimate    = c(sum.ranks.positive = sum.ranks.positive 
                               ,sum.ranks.negative = sum.ranks.negative
                               ,adj.sample.size = adj.sample.size

               ),
               parameter   = null.hypothesis.location,
               p.value     = p.value,
               null.value  = null.hypothesis.location,
               alternative = alternative[1],
               method      = "Wilcoxon Signed-Ranks Test"
               #,conf.int    = c(z.lower,z.upper)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "location"
  names(retval$parameter) <- "null hypothesis location"
  #attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
  
}


wilcoxon.signed.ranks.onesample.test.simple <- median.test.onesample.wilcoxon.simple