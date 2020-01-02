confusion.matrix.true.negative.rate <- function(
  confusion.matrix
) {

confusion.matrix.true.negative.rate.simple(
  true.negative = confusion.matrix.true.negative(confusion.matrix)
  ,false.positive = confusion.matrix.false.positive(confusion.matrix)
) 
}

confusion.matrix.specificity <- confusion.matrix.true.negative.rate
