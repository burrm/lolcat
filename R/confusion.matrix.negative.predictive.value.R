confusion.matrix.negative.predictive.value <- function(
  confusion.matrix
) {
  confusion.matrix.negative.predictive.value.simple(
    true.negative = confusion.matrix.true.negative(confusion.matrix)
    ,false.negative = confusion.matrix.false.negative(confusion.matrix) 
  ) 
}