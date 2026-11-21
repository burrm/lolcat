#' @rdname confusion.matrix.accuracy.simple
confusion.matrix.accuracy <- function(
  confusion.matrix
) {
   confusion.matrix.accuracy.simple(
     true.positive = confusion.matrix.true.positive(confusion.matrix)
     ,false.positive = confusion.matrix.false.positive(confusion.matrix)
     ,true.negative = confusion.matrix.true.negative(confusion.matrix)
     ,false.negative = confusion.matrix.false.negative(confusion.matrix)
   ) 
}
