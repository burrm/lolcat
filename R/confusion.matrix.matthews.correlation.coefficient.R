#' @rdname confusion.matrix.matthews.correlation.coefficient.simple
confusion.matrix.matthews.correlation.coefficient <- function(
  confusion.matrix
) {

confusion.matrix.matthews.correlation.coefficient.simple(
  true.positive = confusion.matrix.true.positive(confusion.matrix)
  ,false.positive = confusion.matrix.false.positive(confusion.matrix)
  ,true.negative = confusion.matrix.true.negative(confusion.matrix)
  ,false.negative = confusion.matrix.false.negative(confusion.matrix)
) 
}

#' @rdname confusion.matrix.matthews.correlation.coefficient.simple
cor.matthews <- confusion.matrix.matthews.correlation.coefficient
