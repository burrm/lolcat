#AKA miss rate, false negative rate
confusion.matrix.false.negative.rate.simple <- function(
  false.negative = 0
  ,true.positive = 1 #optional
  ,count.positive = false.negative + true.positive
) {
  false.negative / count.positive
}

confusion.matrix.miss.rate.simple <- confusion.matrix.false.negative.rate.simple