confusion.matrix.f1.score <- function(
   confusion.matrix
) {
  confusion.matrix.f1.score.simple(
    true.positive = confusion.matrix.true.positive(confusion.matrix)
    ,false.positive = confusion.matrix.false.positive(confusion.matrix)
    ,false.negative = confusion.matrix.false.negative(confusion.matrix)
  )
}