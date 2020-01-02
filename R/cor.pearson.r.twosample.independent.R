cor.pearson.r.twosample.independent <- function(
  x1
  ,x2
  ,x3
  ,x4
  ,alternative = c("two.sided","less","greater"),
  conf.level = .95
) {
  cor.pearson.r.twosample.independent.simple(
    sample.r.g1.g2 = cor(x1,x2),
    sample.size.g1.g2 = length(x1),
    sample.r.g3.g4 = cor(x3,x4),
    sample.size.g3.g4 = length(x3),
    alternative = alternative,
    conf.level = conf.level
  )
  
}