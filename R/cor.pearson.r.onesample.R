cor.pearson.r.onesample <- function(
  x,
  y,
  null.hypothesis.rho = 0,
  alternative = c("two.sided","less","greater"),
  conf.level = .95,
  na.rm = T
) {
  validate.htest.alternative(alternative = alternative)
  if (na.rm) {
    d <- data.frame(x = x, y=y)
    d <- na.omit(d)
    
    x <-d$x
    y <-d$y
  }
  
  cor.pearson.r.onesample.simple(sample.r =  cor(x,y)
                                 , sample.size = length(x)
                                 , null.hypothesis.rho = null.hypothesis.rho
                                 , alternative = alternative
                                 , conf.level = conf.level)
}