#AKA sensitivity, recall, hit rate, true positive rate
confusion.matrix.true.positive.rate.simple <- function(
  true.positive = 0
  ,false.negative = 1 #optional
  ,count.positive = true.positive + false.negative
) {
  true.positive / count.positive
}

confusion.matrix.sensitivity.simple <- confusion.matrix.true.positive.rate.simple
confusion.matrix.recall.simple      <- confusion.matrix.true.positive.rate.simple
confusion.matrix.hit.rate.simple    <- confusion.matrix.true.positive.rate.simple
