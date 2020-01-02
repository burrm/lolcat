confusion.matrix.false.positive.rate <- function(
  confusion.matrix
) {
  confusion.matrix.false.positive.rate.simple(
  false.positive = confusion.matrix.false.positive(confusion.matrix)
  ,true.negative = confusion.matrix.true.negative(confusion.matrix)
  ) 
}

confusion.matrix.fall.out <- confusion.matrix.false.positive.rate