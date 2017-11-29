confusion.matrix.negative.likelihood.ratio <- function(
  confusion.matrix
) {
  confusion.matrix.negative.likelihood.ratio.simple(
    false.negative = confusion.matrix.false.negative(confusion.matrix)
    ,true.positive = confusion.matrix.true.positive(confusion.matrix)
    ,true.negative = confusion.matrix.true.negative(confusion.matrix)
    ,false.positive = confusion.matrix.false.positive(confusion.matrix)
  ) 
}