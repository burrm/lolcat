confusion.matrix.false.omission.rate <- function(
  confusion.matrix
) {
confusion.matrix.false.omission.rate.simple(
  false.negative = confusion.matrix.false.negative(confusion.matrix)
  ,true.negative = confusion.matrix.true.negative(confusion.matrix) 
) 
}
