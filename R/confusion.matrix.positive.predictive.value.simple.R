#AKA precision, positive predictive value
confusion.matrix.positive.predictive.value.simple <- function(
  true.positive = 0
  ,false.positive = 1 
) {
  true.positive / (true.positive + false.positive)
}

confusion.matrix.precision.simple <- confusion.matrix.positive.predictive.value.simple
