sample.size.mean.t.twosample.dependent.dbar <- function(effect.size
                                                   ,variance.diff = 1
                                                   ,alpha = .05
                                                   ,beta = .1
                                                   ,alternative = c("two.sided","less","greater")
                                                   ,details = TRUE
                                                   ,power.from.actual = F #report parameter power instead of true power
) {
  
  ss.1s <- sample.size.mean.t.onesample(
    effect.size = effect.size
    ,variance.est = variance.diff
    ,alpha = alpha
    ,beta = beta
    ,alternative = alternative
  )
  
  ss.1s$type <- c("two.sample dependent")  

  if (details) {
    ss.1s
  }
  else {
    ss.1s$n[1]
  }
  
}


#sample.size.mean.t.twosample.dependent(effect.size = 2, se.est.d = 1)
