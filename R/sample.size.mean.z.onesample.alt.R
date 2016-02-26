sample.size.mean.z.onesample.alt <- function(effect.size
                                             ,se.est = 1
                                             ,conf.level = .95
                                             ,power = .9
                                             ,alternative = c("two.sided","less","greater")
                                             ,details = TRUE
                                             ,power.from.actual = F #report parameter power instead of true power
) {
  sample.size.mean.z.onesample(effect.size = effect.size
                               ,se.est = se.est
                               ,alpha = 1-conf.level
                               ,beta = 1- power
                               ,alternative = alternative
                               ,details = details
                               ,power.from.actual = power.from.actual
  )
}
