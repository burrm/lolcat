confusion.matrix.matthews.correlation.coefficient.simple <- function(
  true.positive = 1
  ,false.positive = 1
  ,true.negative =1
  ,false.negative = 1
) {
  (true.positive * true.negative - false.positive * false.negative) / sqrt((true.positive +false.positive) * (true.positive + false.negative)*(true.negative + false.positive) * (true.negative + false.negative))
}

cor.matthews.simple <- confusion.matrix.matthews.correlation.coefficient.simple