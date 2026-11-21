#' @rdname cor.pearson.r.twosample.dependent.simple
cor.pearson.r.twosample.dependent <- function(
  x1
  ,x2
  ,x3
  ,alternative = c("two.sided","less","greater")
  ,method = c("Steiger", "Hotelling")
  ,conf.level = .95
) {
  # All three correlations must use the same complete observations.
  data <- na.omit(data.frame(x1 = x1, x2 = x2, x3 = x3))
  x1 <- data$x1
  x2 <- data$x2
  x3 <- data$x3
  
  cor.pearson.r.twosample.dependent.simple(
    sample.r.g1.g3 = cor(x1,x3),
    sample.r.g2.g3 = cor(x2,x3),
    sample.r.g1.g2 = cor(x1,x2),
    sample.size = length(x3),
    
    alternative = alternative,
    method = method,
    conf.level = conf.level
  ) 
}
