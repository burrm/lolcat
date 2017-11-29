#accuracy
confusion.matrix.accuracy.simple <- function(
  true.positive = 0
  ,false.positive = 0 #optional
  ,true.negative = 1
  ,false.negative = 0 #optional
  ,count.positive = true.positive + false.negative
  ,count.negative = true.negative + false.positive
) {
  (true.positive + true.negative) / (count.positive + count.negative)
}
