confusion.matrix.positive.likelihood.ratio <- function(
  confusion.matrix
) {
  confusion.matrix.positive.likelihood.ratio.simple(
    true.positive = confusion.matrix.true.positive(confusion.matrix)
    ,false.negative = confusion.matrix.false.negative(confusion.matrix)
    ,false.positive = confusion.matrix.false.positive(confusion.matrix)
    ,true.negative = confusion.matrix.true.negative(confusion.matrix)
  ) 
}