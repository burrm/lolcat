#AKA fall out, false positive rate
confusion.matrix.false.positive.rate.simple <- function(
  false.positive = 0
  ,true.negative = 1 #optional
  ,count.negative = false.positive + true.negative
) {
  false.positive / count.negative
}

confusion.matrix.fall.out.simple <- confusion.matrix.false.positive.rate.simple