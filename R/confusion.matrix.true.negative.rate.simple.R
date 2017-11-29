#AKA specificity, true negative rate
confusion.matrix.true.negative.rate.simple <- function(
  true.negative = 0
  ,false.positive = 1 #optional
  ,count.negative = true.negative + false.positive
) {
  true.negative / count.negative
}

confusion.matrix.specificity.simple <- confusion.matrix.true.negative.rate.simple
