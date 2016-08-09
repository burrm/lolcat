sample.size.mean.z.twosample.independent.alt <- function(effect.size
                                             ,variance = 1
                                             ,conf.level = .95
                                             ,power = .9
                                             ,alternative = c("two.sided","less","greater")
                                             ,details = TRUE
                                             ,power.from.actual = F #report parameter power instead of true power
) {
  sample.size.mean.z.twosample.independent(effect.size = effect.size
                                           ,variance = variance
                                           ,alpha = 1-conf.level
                                           ,beta = 1- power
                                           ,alternative = alternative
                                           ,details = details
                                           ,power.from.actual = power.from.actual
  )
}
