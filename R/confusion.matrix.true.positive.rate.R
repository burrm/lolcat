confusion.matrix.true.positive.rate <- function(
  confusion.matrix
) {
confusion.matrix.true.positive.rate.simple(
  true.positive = confusion.matrix.true.positive(confusion.matrix)
  ,false.negative = confusion.matrix.false.negative(confusion.matrix)
) 
}

confusion.matrix.sensitivity <- confusion.matrix.true.positive.rate
confusion.matrix.recall      <- confusion.matrix.true.positive.rate
confusion.matrix.hit.rate    <- confusion.matrix.true.positive.rate
