confusion.matrix.diagnostic.odds.ratio <- function(
  confusion.matrix
) {
  confusion.matrix.diagnostic.odds.ratio.simple(
    true.positive = confusion.matrix.true.positive(confusion.matrix)
    ,false.negative = confusion.matrix.false.negative(confusion.matrix)
    ,false.positive = confusion.matrix.false.positive(confusion.matrix)
    ,true.negative = confusion.matrix.true.negative(confusion.matrix) 
  )
}