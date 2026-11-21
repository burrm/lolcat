#' @rdname confusion.matrix.markedness.simple
confusion.matrix.markedness <- function(
  confusion.matrix
) {

confusion.matrix.markedness.simple(
  #PPV inputs
  true.positive = confusion.matrix.true.positive(confusion.matrix)
  ,false.positive = confusion.matrix.false.positive(confusion.matrix)
  
  #NPV inputs
  ,true.negative = confusion.matrix.true.negative(confusion.matrix)
  ,false.negative = confusion.matrix.false.negative(confusion.matrix)
) 
}