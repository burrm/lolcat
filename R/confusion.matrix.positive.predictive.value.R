confusion.matrix.positive.predictive.value <- function(
  confusion.matrix
) {
confusion.matrix.positive.predictive.value.simple(
  true.positive = confusion.matrix.true.positive(confusion.matrix)
  ,false.positive = confusion.matrix.false.positive(confusion.matrix) 
) 
}

confusion.matrix.precision <- confusion.matrix.positive.predictive.value