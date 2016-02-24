cor.pearson.r.onesample <- function(
  x,
  y,
  h0.rho = 0,
  alternative = c("two-sided","less","greater"),
  conf.level = .95
) {
  cor.pearson.r.onesample.simple(sample.r =  cor(x,y)
                                 , sample.size = length(x)
                                 , h0.rho = h0.rho
                                 , alternative = alternative
                                 , conf.level = conf.level)
}