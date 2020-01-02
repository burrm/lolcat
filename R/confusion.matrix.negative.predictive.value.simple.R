confusion.matrix.negative.predictive.value.simple <- function(
  true.negative = 0
  ,false.negative = 1 
) {
  true.negative / (true.negative + false.negative)
}

