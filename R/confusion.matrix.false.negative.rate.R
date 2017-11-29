confusion.matrix.false.negative.rate <- function(
  confusion.matrix
) {
confusion.matrix.false.negative.rate.simple(
  false.negative = confusion.matrix.false.negative(confusion.matrix)
  ,true.positive = confusion.matrix.true.positive(confusion.matrix)
) 
  
}

confusion.matrix.miss.rate <- confusion.matrix.false.negative.rate