#' @rdname cor.pearson.r.twosample.independent.simple
cor.pearson.r.twosample.independent <- function(
  x1
  ,x2
  ,x3
  ,x4
  ,alternative = c("two.sided","less","greater"),
  conf.level = .95
) {
  # The two independent samples can have different lengths and missing rows.
  data.g1.g2 <- na.omit(data.frame(x1 = x1, x2 = x2))
  data.g3.g4 <- na.omit(data.frame(x3 = x3, x4 = x4))
  x1 <- data.g1.g2$x1
  x2 <- data.g1.g2$x2
  x3 <- data.g3.g4$x3
  x4 <- data.g3.g4$x4

  cor.pearson.r.twosample.independent.simple(
    sample.r.g1.g2 = cor(x1,x2),
    sample.size.g1.g2 = length(x1),
    sample.r.g3.g4 = cor(x3,x4),
    sample.size.g3.g4 = length(x3),
    alternative = alternative,
    conf.level = conf.level
  )
  
}
