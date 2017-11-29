confusion.matrix.f1.score.simple <- function(
  true.positive = 1
  ,false.positive = 0
  ,false.negative = 0
) {
  (2*true.positive)/ (2*true.positive + false.positive + false.negative)
}